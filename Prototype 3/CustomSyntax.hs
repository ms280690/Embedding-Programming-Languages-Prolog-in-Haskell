{-# LANGUAGE  DeriveDataTypeable,
              ViewPatterns,
              ScopedTypeVariables,
              FlexibleInstances,
              DefaultSignatures,
              TypeOperators,
              FlexibleContexts,
              TypeFamilies,
              DataKinds,
              OverlappingInstances,
              DataKinds,
              PolyKinds,
              TypeOperators,
              LiberalTypeSynonyms,
              TemplateHaskell,
              RankNTypes,
              AllowAmbiguousTypes,
              MultiParamTypeClasses,
              FunctionalDependencies,
              ConstraintKinds,
              ExistentialQuantification
              #-}

module CustomSyntax where

import Data.Generics (Data(..), Typeable(..))
import Data.List (intercalate)
import Data.Char (isLetter)

import Control.Monad.State.UnificationExtras
import Control.Unification as U


import Data.Functor.Fixedpoint as DFF


import Control.Unification.IntVar
import Control.Unification.STVar as ST

import Control.Unification.Ranked.IntVar
import Control.Unification.Ranked.STVar

import Control.Unification.Types as UT



import Data.Traversable as T
import Data.Functor
import Data.Foldable
import Control.Applicative


import Data.List.Extras.Pair
import Data.Map as Map
import Data.Set as S


import Control.Monad.Error
import Control.Monad.Trans.Except


import Prolog
import PrologFlat
import VariableHandler
import Translators

{--
data FTS a = forall a . FV Id | FS Atom [a] deriving (Eq, Show, Ord, Typeable)

newtype Prolog = P (Fix FTS) deriving (Eq, Show, Ord, Typeable)

unP :: Prolog -> Fix FTS
unP (P x) = x

instance Functor FTS where
    fmap = T.fmapDefault

instance Foldable FTS where
    foldMap = T.foldMapDefault

instance Traversable FTS where
    traverse f (FS atom xs) = FS atom <$> sequenceA (Prelude.map f xs)
    traverse _ (FV v) = pure (FV v)

instance Unifiable FTS where
    zipMatch (FS al ls) (FS ar rs) = if (al == ar) && (length ls == length rs)
                    then FS al <$> pairWith (\l r -> Right (l,r)) ls rs
                    else Nothing
    zipMatch (FV v1) (FV v2) = if (v1 == v2) then Just (FV v1)
        else Nothing
    zipMatch _ _ = Nothing

instance Applicative FTS where
    pure x = FS "" [x]
    (FS a fs) <*> (FS b xs)   = FS (a ++ b) [f x | f <- fs, x <- xs]
    --other cases
{--
instance Monad FTS where
    func =
instance Variable FTS where
    func =

instance BindingMonad FTS where
    func =
--}
--}

-- data VariableName = VariableName Int String

-- idToVariableName :: Id -> VariableName
-- idToVariableName (i, s) = VariableName i s

-- variablenameToId :: VariableName -> Id
-- variablenameToId (VariableName i s) = (i,s)

-- termFlattener :: Term -> Fix FTS
-- termFlattener (Var v)           =   DFF.Fix $ FV v
-- termFlattener (Struct a xs)     =   DFF.Fix $ FS a (Prelude.map termFlattener xs)

-- unFlatten :: Fix FTS -> Term
-- unFlatten (DFF.Fix (FV v))      =   Var v
-- unFlatten (DFF.Fix (FS a xs))   =   Struct a (Prelude.map unFlatten xs)


-- variableExtractor :: Fix FTS -> [Fix FTS]
-- variableExtractor (Fix x) = case x of
--   (FS _ xs)   ->  Prelude.concat $ Prelude.map variableExtractor xs
--   (FV v)     ->  [Fix $ FV v]
-- --  _       ->  []

-- variableIdExtractor :: Fix FTS -> [Id]
-- variableIdExtractor (Fix x) = case x of
--  (FS _ xs) -> Prelude.concat $ Prelude.map variableIdExtractor xs
--  (FV v) -> [v]

-- {--
-- variableNameExtractor :: Fix FTS -> [VariableName]
-- variableNameExtractor (Fix x) = case x of
--   (FS _ xs) -> Prelude.concat $ Prelude.map variableNameExtractor xs
--   (FV v)     -> [v]
--   _         -> []
-- --}

-- variableSet :: [Fix FTS] -> S.Set (Fix FTS)
-- variableSet a = S.fromList a

-- variableNameSet :: [Id] -> S.Set (Id)
-- variableNameSet a = S.fromList a


-- varsToDictM :: (Ord a, Unifiable t) =>
--     S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
-- varsToDictM set = foldrM addElt Map.empty set where
--   addElt sv dict = do
--     iv <- freeVar
--     return $! Map.insert sv iv dict


-- instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}

-- test1 ::
--   ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
--            (ST.STBinding s)
--             (UT.UTerm (FTS) (ST.STVar s (FTS)),
--              Map Id (ST.STVar s (FTS)))
-- test1 = do
--     let
--         t1a = (Fix $ FV $ (0, "x"))
--         t2a = (Fix $ FV $ (1, "y"))
--     (x1,d1) <- lift . translateToUTerm $ t1a --error
--     (x2,d2) <- lift . translateToUTerm $ t2a
--     x3 <- U.unify x1 x2
--     return (x3, d1 `Map.union` d2)


-- test2 ::
--   ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
--            (ST.STBinding s)
--             (UT.UTerm (FTS) (ST.STVar s (FTS)),
--              Map Id (ST.STVar s (FTS)))
-- test2 = do
--     let
--         t1a = (Fix $ FS "a" [Fix $ FV $ (0, "x")])
--         t2a = (Fix $ FV $ (1, "y"))
--     (x1,d1) <- lift . translateToUTerm $ t1a --error
--     (x2,d2) <- lift . translateToUTerm $ t2a
--     x3 <- U.unify x1 x2
--     return (x3, d1 `Map.union` d2)


-- test3 ::
--   ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
--            (ST.STBinding s)
--             (UT.UTerm (FTS) (ST.STVar s (FTS)),
--              Map Id (ST.STVar s (FTS)))
-- test3 = do
--     let
--         t1a = (Fix $ FS "a" [Fix $ FV $ (0, "x")])
--         t2a = (Fix $ FV $ (0, "x"))
--     (x1,d1) <- lift . translateToUTerm $ t1a --error
--     (x2,d2) <- lift . translateToUTerm $ t2a
--     x3 <- U.unify x1 x2
--     return (x3, d1 `Map.union` d2)
-- {--
-- goTest test3
-- "ok:    STVar -9223372036854775807
-- [(VariableName 0 \"x\",STVar -9223372036854775808)]"
-- --}

-- test4 ::
--   ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
--            (ST.STBinding s)
--             (UT.UTerm (FTS) (ST.STVar s (FTS)),
--              Map Id (ST.STVar s (FTS)))
-- test4 = do
--     let
--         t1a = (Fix $ FS "a" [Fix $ FV $ (0, "x")])
--         t2a = (Fix $ FV $ (0, "x"))
--     (x1,d1) <- lift . translateToUTerm $ t1a --error
--     (x2,d2) <- lift . translateToUTerm $ t2a
--     x3 <- U.unifyOccurs x1 x2
--     return (x3, d1 `Map.union` d2)
-- {--
-- goTest test4
-- "ok:    STVar -9223372036854775807
-- [(VariableName 0 \"x\",STVar -9223372036854775808)]"
-- --}

-- test5 ::
--   ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
--            (ST.STBinding s)
--             (UT.UTerm (FTS) (ST.STVar s (FTS)),
--              Map Id (ST.STVar s (FTS)))
-- test5 = do
--     let
--         t1a = (Fix $ FS "a" [Fix $ FV $ (0, "x")])
--         t2a = (Fix $ FS "b" [Fix $ FV $ (0, "y")])
--     (x1,d1) <- lift . translateToUTerm $ t1a --error
--     (x2,d2) <- lift . translateToUTerm $ t2a
--     x3 <- U.unify x1 x2
--     return (x3, d1 `Map.union` d2)

-- goTest :: (Show b) => (forall s .
--   (ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
--            (ST.STBinding s)
--             (UT.UTerm (FTS) (ST.STVar s (FTS)),
--              Map Id (ST.STVar s (FTS))))) -> String
-- goTest test = ST.runSTBinding $ do
--   answer <- runErrorT $ test
--   return $! case answer of
--     (Left x)  -> "error: " ++ show x
--     (Right y) -> "ok:    " ++ show y


---------------------------------------------------------------
---------------------------------------------------------------
---------------GLUE-CODE---------------------------------------
{--
monadicUnify :: Term -> Term -> ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS)))
monadicUnify t1 t2 = do
    let
        t1f = termFlattener t1
        t2f = termFlattener t2
    (x1,d1) <- lift . translateToUTerm $ t1f
    (x2,d2) <- lift . translateToUTerm $ t2f
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)

--}

-- type Subst = Id -> Term

-- Convert result from monadicUnify to [Subst]
{--
goMonadicTest :: (Show b) => (forall s .
  (ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS))))) -> [Subst]
goMonadicTest test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)  -> [nullSubst]
    (Right y) -> convertToSubst y
--}

--(Id, STVar s FTS)
--convertToSubst :: Map Id (ST.STVar s FTS) -> [(Id, ST.STVar s FTS)]
{--
convertToSubst m = convertToSubst1 Map.toAscList m

convertToSubst1 (id, ST.STVar _ fts):xs = (id, (unFlatten fts)) : convertToSubst1 xs
--}
