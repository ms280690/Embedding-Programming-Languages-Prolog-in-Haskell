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
        AllowAmbiguousTypes
        #-}

{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}

module RunAndExtract where

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
import Translators

instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}

runTest :: (Show b) => (forall s .
	(ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
         (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm))))) -> String
runTest test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)  -> "error: " ++ show x
    (Right y) -> "ok:    " ++ show y

monadicUnification :: (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s)) => (forall s. ((Fix FlatTerm) -> (Fix FlatTerm) ->
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s) (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
            Map VariableName (ST.STVar s (FlatTerm)))))
monadicUnification t1 t2 = do
  (x1,d1) <- lift . translateToUTerm $ t1
  (x2,d2) <- lift . translateToUTerm $ t2
  x3 <- U.unify x1 x2
  return $! (x3, d1 `Map.union` d2)

runUnify ::
  (forall s. (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s))
  =>
      (ErrorT
          (UT.UFailure FlatTerm (ST.STVar s FlatTerm))
          (ST.STBinding s)
          (UT.UTerm FlatTerm (ST.STVar s FlatTerm),
             Map VariableName (ST.STVar s FlatTerm)))
     )
  -> [(VariableName, Prolog)]
runUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test --ERROR
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> extractUnifier dict

extractUnifier ::
  (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s))
  => (forall s. Map VariableName (STVar s FlatTerm)
      -> (ST.STBinding s [(VariableName, Prolog)])
     )
extractUnifier dict = do
  let ld1 = Map.toList dict
  ld2 <- Control.Monad.Error.sequence [ v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3, let v = translateFromUTerm dict v2 ]
  return ld4
