module VariableHandler where

import           Data.Functor.Fixedpoint   as DFF
import qualified Data.Set                  as S
import           Control.Unification.STVar as ST
import           Data.Map                  as Map
import           Control.Unification       as U
import           Data.Foldable             as DF
import           PrologLanguage


variableExtractor :: Fix FlatTerm -> [Fix FlatTerm]
variableExtractor (Fix x) = case x of
    (Struct _ xs)   ->  Prelude.concat $ Prelude.map variableExtractor xs
    (Var v)         ->  [Fix $ Var v]
    _               ->  []

variableNameExtractor :: Fix FlatTerm -> [VariableName]
variableNameExtractor (Fix x) = case x of
    (Struct _ xs)   -> Prelude.concat $ Prelude.map variableNameExtractor xs
    (Var v)         -> [v]
    _               -> []

variableSet :: [Fix FlatTerm] -> S.Set (Fix FlatTerm)
variableSet a = S.fromList a

variableNameSet :: [VariableName] -> S.Set (VariableName)
variableNameSet a = S.fromList a

varsToDictM :: (Ord a, Unifiable t) =>
    S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
varsToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- freeVar
    return $! Map.insert sv iv dict
