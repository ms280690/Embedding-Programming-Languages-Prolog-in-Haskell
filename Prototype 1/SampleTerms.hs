module SampleTerms where

import Data.Functor.Fixedpoint as DFF

import PrologLanguage

fix1 = (Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), 
	(Fix Wildcard), (Fix $ Cut 0), (Fix $ Struct "b" 
		[(Fix $ Var $ VariableName 1 "y"), (Fix Wildcard), 
		(Fix $ Cut 1), (Fix $ Struct "c" [(Fix $ Var $ VariableName 2 "z"), 
			(Fix Wildcard), (Fix $ Cut 2), (Fix $ Struct "d" [])])])])


fix2 = Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), (Fix $ Cut 0), 
		(Fix $ Wildcard)]

fix3 = (Fix $ Var $ VariableName 1 "x")

fix4 = (Fix $ Var $ VariableName 2 "y")
