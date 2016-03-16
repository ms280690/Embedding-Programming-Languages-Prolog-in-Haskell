module PrologLanguage where

import Data.Functor.Fixedpoint as DFF
import Data.Traversable as T
import Data.Foldable as DF
import Control.Applicative ((<$>),(<*>),pure,Applicative)
import Control.Unification as U
import Data.List.Extras.Pair

-- | Atoms are represented as Strings.
type Atom = String

-- | Variables are pairs of value of type Int and name of type String.
data VariableName = VariableName Int String  deriving (Show, Eq, Ord)

data FlatTerm a = 
					Struct Atom [a]
				| 	Var VariableName
				|	Wildcard
				|	Cut Int deriving (Show, Eq, Ord)

data Prolog = P (Fix FlatTerm) deriving (Show,Eq,Ord)

instance Functor (FlatTerm) where
	fmap 							= T.fmapDefault

instance Foldable (FlatTerm) where
 	foldMap 						= T.foldMapDefault

instance Traversable (FlatTerm) where
  	traverse f (Struct atom x)		=	Struct atom <$> sequenceA (Prelude.map f x)
  	traverse _ (Var v)				=	pure (Var v)
  	traverse _ Wildcard				=	pure Wildcard
  	traverse _ (Cut i)				= 	pure (Cut i)

instance Unifiable (FlatTerm) where
	zipMatch (Struct al ls) (Struct ar rs) = 
		if (al == ar) && (length ls == length rs) 
			then Struct al <$> pairWith (\l r -> Right (l,r)) ls rs  		
			else Nothing
	zipMatch Wildcard _ = Just Wildcard
	zipMatch _ Wildcard = Just Wildcard
	zipMatch (Cut i1) (Cut i2) = if (i1 == i2) 
		then Just (Cut i1) 
		else Nothing

instance Applicative (FlatTerm) where
	pure x 									= 	Struct "" [x] 
	_ 				<*> 	Wildcard		= 	Wildcard
	_				<*> 	(Cut i) 		= 	Cut i
	_				<*>		(Var v)			=	(Var v)
	(Struct a fs)	<*> 	(Struct b xs) 	= Struct (a ++ b) [f x | f <- fs, x <- xs] 