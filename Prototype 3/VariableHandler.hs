module VariableHandler where

import Data.Functor.Fixedpoint as DFF

import qualified Data.Set as S

import Control.Unification.STVar as ST 

import Data.Map as Map

import Control.Unification as U

import Data.Foldable as DF

import PrologFlat

import Prolog


data VariableName = VariableName Int String

idToVariableName :: Id -> VariableName
idToVariableName (i, s) = VariableName i s

variablenameToId :: VariableName -> Id
variablenameToId (VariableName i s) = (i,s)

termFlattener :: Term -> Fix FTS
termFlattener (Var v)           =   DFF.Fix $ FV v
termFlattener (Struct a xs)     =   DFF.Fix $ FS a (Prelude.map termFlattener xs)

unFlatten :: Fix FTS -> Term
unFlatten (DFF.Fix (FV v))      =   Var v
unFlatten (DFF.Fix (FS a xs))   =   Struct a (Prelude.map unFlatten xs)


variableExtractor :: Fix FTS -> [Fix FTS]
variableExtractor (Fix x) = case x of
  (FS _ xs)   ->  Prelude.concat $ Prelude.map variableExtractor xs
  (FV v)     ->  [Fix $ FV v]
--  _       ->  [] 

variableIdExtractor :: Fix FTS -> [Id]
variableIdExtractor (Fix x) = case x of
	(FS _ xs) -> Prelude.concat $ Prelude.map variableIdExtractor xs
	(FV v) -> [v]

{--
variableNameExtractor :: Fix FTS -> [VariableName]
variableNameExtractor (Fix x) = case x of
  (FS _ xs) -> Prelude.concat $ Prelude.map variableNameExtractor xs
  (FV v)     -> [v]
  _         -> [] 
--}

variableSet :: [Fix FTS] -> S.Set (Fix FTS)
variableSet a = S.fromList a

variableNameSet :: [Id] -> S.Set (Id)
variableNameSet a = S.fromList a


varsToDictM :: (Ord a, Unifiable t) =>
    S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
varsToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- freeVar
    return $! Map.insert sv iv dict