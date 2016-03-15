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

module PrologFlat where

import Data.Generics (Data(..), Typeable(..))
import Data.Functor.Fixedpoint as DFF
import Data.Traversable as T 
import Data.Functor 
import Data.Foldable
import Control.Applicative
import Control.Unification as U
import Data.List.Extras.Pair

import Prolog

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
       traverse _ (FV v) =  pure (FV v)

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
