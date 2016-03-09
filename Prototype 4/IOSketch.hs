module IOSketch (
  -- * The Mini Language
  Language(..)
  , Statement(..)
  , Expr(..)
  , Op(..)
  , Variable
    -- * The (Im)pure Result type
  , Result (..)
  , XExpr
    -- * Helper Types
  , Map.Map
  , State
  , XResult
    -- * Functions
    -- ** Top level functions
  , runIO
  , runProg
    -- ** Intermediate semantic functions
  , go
  , reduceE
  , runStat
    -- ** helper functions (these can be hidden)
  , unwind
  , ioPrompt
  )
where

import Data.Map as Map
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

-- | 'Result's are the pure partial result of executing a Program
-- produced by `runProg'.  A result is essentially a list of Int's (with
-- constructors Empty and Cons).  However, it may also be a suspended
-- computation 'Read io k' where 'io' is an IO computation that
-- yields an integer, and k is an Int -> Result continuation.

-- XResult is essentially a backwards list of Ints.


-- | XExpr is a concrete representation of the continuation of evaluating
-- an integer expression.  Given an XExpr, a State (current state of memory),
-- and a partial list of results, 'go' produces an honest (Int -> Result)
-- continuation.

--XExpr
--Assign2           -- ^ Continue by assigning to Variable, then evaluating the
           -- remainining statements

--Yield2            -- ^ Continue by yielding an Int value, then evaluating the
           -- remainining statements

--Op2A           -- ^ Continue by evaluting the second operand of a binary
           -- expression

--Op2B           -- ^ After evaluting the second operand of a binary
           -- expression, continue by evaluating the operation
data Language = Progr [Statement]
data Statement = Assign Variable Expr
               | Yield Expr

data Expr = Lit Int
          | Var Variable
          | Op Op Expr Expr
          | Prompt String

data Op = Plus | Minus | Times

type Variable = String
type State = Map.Map Variable Int

data Result = Empty
            | Cons Int Result
            | Read (IO Int) (Int -> Result)

data XResult = XEmpty
             | XCons Int XResult

data XExpr = Assign2 Variable [Statement]
           | Yield2 [Statement]
           | Op2A Op Expr XExpr
           | Op2B Op Int  XExpr



-- | Run in the IO monad, runIO converts a Result to a list of Int's.
runIO :: Result -> IO [Int]
runIO Empty           = return []
runIO (Cons i r)      = (i:) <$> runIO r
runIO (Read p k)      = p >>= runIO . k

go :: XExpr -> State -> XResult -> Int -> Result
go x s xr i = case x of
  Op2B o i1 x2 -> go x2 s xr $ case o of
      Plus  -> i1 + i
      Minus -> i1 - i
      Times -> i1 * i
  Op2A o e2 x2 -> reduceE e2 (Op2B o i x2) s xr
  Assign2 v ss -> runStat ss (Map.insert v i s) xr
  Yield2    ss -> runStat ss s (XCons i xr)

reduceE :: Expr -> XExpr -> State -> XResult -> Result
reduceE e k state xr = case e of
  Lit ell -> go k state xr ell
  Var v   -> go k state xr . maybe 0 id . Map.lookup v $ state
  Op o e1 e2 -> reduceE e1 (Op2A o e2 k) state xr
  Prompt p  -> Read (ioPrompt p) (go k state xr)

runProg :: Language -> Result
runProg (Progr ll) = runStat ll (Map.empty) XEmpty

runStat :: [Statement] -> State -> XResult -> Result
runStat [] _ xr = unwind xr Empty
runStat (Assign v e:ss) state xr = reduceE e (Assign2 v ss) state xr
runStat (Yield e:ss)    state xr = reduceE e (Yield2 ss) state xr

unwind :: XResult -> Result -> Result
unwind XEmpty y = y
unwind (XCons i x) y = unwind x (Cons i y)

ioPrompt :: String -> IO Int
ioPrompt prompt = do { putStr $ prompt ++ " "; x <- getLine ; return (read x) }
