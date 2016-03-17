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

import           Control.Applicative ((<$>),(<*>),pure,Applicative)
import           Control.Monad.Error
import           Control.Monad.Trans.Except
import           Control.Unification       as U
import           Control.Unification.STVar as ST
import           Control.Unification.Types as UT
import           Data.Foldable             as DF
import           Data.Functor.Fixedpoint   as DFF
import           Data.Generics (Data(..), Typeable(..))
import           Data.List.Extras.Pair
import           Data.Map                  as Map
import qualified Data.Set                  as S
import           Data.Traversable          as T
import           Prolog
import           PrologFlat
import           Translators
import           VariableHandler


instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}

runTest :: (Show b) => (forall s .
  (ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS))))) -> String
runTest test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)  -> "error: " ++ show x
    (Right y) -> "ok:    " ++ show y


monadicUnification :: (BindingMonad FTS (STVar s FTS) (ST.STBinding s)) => (forall s. (Term -> Term -> ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)),
            Map Id (ST.STVar s (FTS)))))
monadicUnification t1 t2 = do
  let
    t1f = termFlattener t1
    t2f = termFlattener t2
  (x1,d1) <- lift . translateToUTerm $ t1f
  (x2,d2) <- lift . translateToUTerm $ t2f
  x3 <- U.unify x1 x2
  --get state from somehwere, state -> dict
  return $! (x3, d1 `Map.union` d2)


runUnify ::
  (forall s. (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  =>
      (ErrorT
          (UT.UFailure FTS (ST.STVar s FTS))
          (ST.STBinding s)
          (UT.UTerm FTS (ST.STVar s FTS),
             Map Id (ST.STVar s FTS)))
     )
  -> [(Id, Prolog)]
runUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test --ERROR
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> extractUnifier dict


extractUnifier ::
  (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  => (forall s. Map Id (STVar s FTS)
      -> (ST.STBinding s [(Id, Prolog)])
     )
extractUnifier dict = do
  let ld1 = Map.toList dict
  ld2 <- Control.Monad.Error.sequence [ v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3, let v = translateFromUTerm dict v2 ]
  return ld4

