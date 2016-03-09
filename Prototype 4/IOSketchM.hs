module IOSketchM
where

import Data.Map as Map

import Control.Monad.Trans(lift)
import qualified Control.Monad.Trans.Except as CME
import qualified Control.Monad.Trans.State  as CMS
import qualified Control.Monad.Trans.Cont   as CMC
import Control.Applicative
-- | Language is a mini language that has two kinds of
-- statements: 'Assign' which assign a value to an expression, and
-- Yield, which evaluates an expression and produces a result.
--
-- The result of fully running a program is a finite list of Ints, one
-- for each Yield.  The language has side effects, both through variable
-- assignment and (IO) through prompting the user for values.

-- | 'Expr'essions are either literals, variables,
-- binary expressions (+,-,*), or prompt expressions, which are evaluated
-- by using a prompt string and reading from the terminal.

-- unassigned variables have value 0

-- | 'Result's are the pure partial result of executing a Program
-- produced by `runProg'.  A result is essentially a list of Int's (with
-- constructors Empty and Cons).  However, it may also be a suspended
-- computation 'Read io k' where 'io' is an IO computation that
-- yields an integer, and k is an Int -> Result continuation.

data Language = Progr [Statement]
data Statement = Assign Variable Expr
               | Yield Expr

data Expr = Lit Int
          | Var Variable
          | Op Op Expr Expr
          | Prompt String

data Op = Plus | Minus | Times

opFun :: Num a => Op -> a -> a -> a
opFun Plus      = (+)
opFun Minus     = (-)
opFun Times     = (*)

type Variable = String
type Dict = Map.Map Variable Int

getVarDict :: Variable -> Dict -> Int
getVarDict v = maybe 0 id . Map.lookup v

setVarDict :: Variable -> Int -> Dict -> Dict
setVarDict v i = Map.insert v i

data Result = Empty
            | Cons Int Result
            | Read (IO Int) (Int -> Result)

-- | Run in the IO monad, runIO converts a Result to a list of Int's.

-- | XM is the monad in which we evaluate language to get results.
-- It's a continuation monad around a state monad around an exception
-- handling monad.  The exception handling monad is there so that Prompt
-- expressions can immediately escape to result in a Read Result.
-- The continuation monad is there so that the current continuation can be
-- captured, when dealing with Prompt expressions.
-- The State monad is standard variable handling.

-- necessary instance declarations.

-- functions for plumbing.  The first four unwind monads to results

runIO :: Result -> IO [Int]
runIO Empty           = return []
runIO (Cons i r)      = (i:) <$> runIO r
runIO (Read p k)      = p >>= runIO . k

newtype XM a = XM {
  unXM :: CMC.ContT Result
          (CMS.StateT Dict (CME.Except Result)) a }

instance Monad XM where
  return        = XM . return
  (>>=) m f     = XM $ unXM m >>= (unXM . f)

instance Applicative XM where
  pure  = return
  (<*>) f a = do { ff <- f ; aa <- a ; return $ ff aa }

instance Functor XM where
  fmap f m = m >>= (return . f)


runEE :: CME.Except Result Result -> Result
runEE m = either id id . CME.runExcept $ m

runSS
  :: (CMS.StateT Dict (CME.Except Result)) Result
     -> Dict
     -> Result
runSS m s = runEE (m `CMS.evalStateT` s)

runKK
  :: CMC.ContT Result (CMS.StateT Dict (CME.Except Result)) Result
     -> Dict
     -> Result
runKK m s = (m `CMC.runContT` (return . id)) `runSS` s

runXM :: XM Result -> Dict -> Result
runXM = runKK . unXM

-- Specific functionality for the XM monad.

-- | getVar.  return a Variable's current value in the store.
-- Unassigned variables return 0 (see getVarDict).

-- | setVar.  set a Variable's current value in the store.

-- | escape.  Generate a result which is apparently an XM Int
-- but really throws an exception out so that runXM generate a
-- Read result.

-- The trick is converting the captured expression to the right
-- form for throwing.  The captured continuation k has type
-- (Int -> XM Result).  Note that the continuation passed out
-- captures the current state.

getVar :: Variable -> XM Int
getVar v = XM . lift . CMS.gets $ (getVarDict v)

setVar :: Variable -> Int -> XM ()
setVar v i = XM . lift . CMS.modify' $ (setVarDict v i)

getState :: XM Dict
getState = XM (lift CMS.get)

throw :: Result -> XM a
throw = XM . lift . lift . CME.throwE

callCC :: ((a -> XM b) -> XM a) -> XM a
callCC f = XM $ CMC.callCC (unXM . f . (XM .))

pureCont :: (a -> XM Result) -> Dict -> (a -> Result)
pureCont kk ss v = (kk v) `runXM` ss


immedRead :: String -> XM Int
immedRead str = callCC $ \ k -> do
  state <- getState
  throw $ Read (ioPrompt str) (pureCont k state)


{-
 ---------------------------------------------------------------------
 Language interpretation
 ---------------------------------------------------------------------
-}

interpExpr :: Expr -> XM Int
interpExpr e = case e of
  Lit ell       -> return ell
  Var v         -> getVar v
  Op o e1 e2    -> do
    i1 <- interpExpr e1
    i2 <- interpExpr e2
    return $! opFun o i1 i2
  Prompt p      -> immedRead p

interpStat :: [Statement] -> XM Result
interpStat []       = return Empty
interpStat (s:ss)   = case s of
  Assign v e    -> do
    i <- interpExpr e
    setVar v i
    interpStat ss
  Yield e       -> do
    i <- interpExpr e
    b <- interpStat ss
    return $! Cons i b

runProg :: Language -> Result
runProg (Progr ll) = runXM (interpStat ll) Map.empty

{-
 ---------------------------------------------------------------------
 Helper functions
 ---------------------------------------------------------------------
-}


ioPrompt :: String -> IO Int
ioPrompt prompt = do { putStr $ prompt ++ " "; x <- getLine ; return (read x) }

{-
 ---------------------------------------------------------------------
 A sample program
 ---------------------------------------------------------------------
-}

testProg :: Language
testProg = let
  is (Var x) y = Assign x y
  is _ _ = error "`is` needs a Var on the left"
  yield = Yield
  varA = Var "a"
  varB = Var "b"
  varC = Var "c"
  varD = Var "d"
  three = Lit 3
  five  = Lit 5
  prompt = Prompt
  (+) = Op Plus
  (-) = Op Minus
  (*) = Op Times
  infixl 7 *
  infixl 6 +
  infixl 6 -
  infix 0 `is`
  in Progr
     [
       varA `is` prompt "a? "
     , varB `is` prompt "b? "
     , varC `is` prompt "c? "
     , yield varA
     , yield varB
     , yield varC
     , varD `is` three * varA - varB * five + varC
     , yield varD
     , varB `is` prompt "b? "
     , varD `is` three * varA - five * varB + varC
     , yield varD
    ]

