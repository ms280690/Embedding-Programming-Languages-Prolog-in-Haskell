{-# LANGUAGE 	DeriveDataTypeable, 
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
				AllowAmbiguousTypes
				#-}

{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}

module STVarExperiment where

import Data.Generics (Data(..), Typeable(..))	

import Data.Functor.Fixedpoint as DFF

import Data.Traversable as T

import Data.Foldable as DF

import Control.Applicative ((<$>),(<*>),pure,Applicative)

import Data.List.Extras.Pair

import Control.Unification as U

import Control.Unification.Types as UT

import Control.Unification.STVar as ST 

import qualified Data.Set as S

import Data.Map as Map

import Control.Monad.Trans.Except

import Control.Monad.Error

import PrologLanguage

import VariableHandler

import Translators

import TestFunctions

import RunAndExtract

fix1 = (Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), 
	(Fix Wildcard), (Fix $ Cut 0), (Fix $ Struct "b" 
		[(Fix $ Var $ VariableName 1 "y"), (Fix Wildcard), 
		(Fix $ Cut 1), (Fix $ Struct "c" [(Fix $ Var $ VariableName 2 "z"), 
			(Fix Wildcard), (Fix $ Cut 2), (Fix $ Struct "d" [])])])])


fix2 = Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), (Fix $ Cut 0), 
		(Fix $ Wildcard)]

fix3 = (Fix $ Var $ VariableName 1 "x")

fix4 = (Fix $ Var $ VariableName 2 "y")
