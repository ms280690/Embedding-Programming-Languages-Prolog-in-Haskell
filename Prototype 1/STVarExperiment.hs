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


stExtract ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
stExtract = do
	(x, d) <- lift . translateToUTerm $ fix2
	return (x, d)

goTest :: (Show b) => (forall s . 
	(ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm))))) -> String
goTest test = ST.runSTBinding $ do
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


goUnify ::
  (forall s. (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s))
  =>
      (ErrorT
          (UT.UFailure FlatTerm (ST.STVar s FlatTerm))
          (ST.STBinding s)
          (UT.UTerm FlatTerm (ST.STVar s FlatTerm),
             Map VariableName (ST.STVar s FlatTerm)))
     )
  -> [(VariableName, Prolog)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test --ERROR
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> f1 dict


f1 ::
  (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s))
  => (forall s. Map VariableName (STVar s FlatTerm)
      -> (ST.STBinding s [(VariableName, Prolog)])
     )
f1 dict = do
  let ld1 = Map.toList dict
  ld2 <- Control.Monad.Error.sequence [ v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3, let v = translateFromUTerm dict v2 ]
  return ld4

fix1 = (Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), 
	(Fix Wildcard), (Fix $ Cut 0), (Fix $ Struct "b" 
		[(Fix $ Var $ VariableName 1 "y"), (Fix Wildcard), 
		(Fix $ Cut 1), (Fix $ Struct "c" [(Fix $ Var $ VariableName 2 "z"), 
			(Fix Wildcard), (Fix $ Cut 2), (Fix $ Struct "d" [])])])])


fix2 = Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), (Fix $ Cut 0), 
		(Fix $ Wildcard)]

fix3 = (Fix $ Var $ VariableName 1 "x")

fix4 = (Fix $ Var $ VariableName 2 "y")


{--
prettyDisplay (UT.UTerm x,  m)  =   show x ++ " " ++ variableMapPrinter m
prettyDisplay (UT.UVar  v,  m)  =   show v ++ " " ++ variableMapPrinter m 

variableMapPrinter :: (Show a, Show k) => Map k a -> String
variableMapPrinter m = show $ Map.toList m


instance Show VariableName where
  show (VariableName i (x:xs)) = "Var " ++ show ((toUpper x) : xs)  
                      ++ " Val " ++ show i    

instance Show (Fix FlatTerm) where
  show (Fix (Struct atom xs)) = " Struct " ++ atom ++ " ["  ++ 
                  Prelude.concat (Prelude.map show xs) ++ "] "                
  show (Fix (Var v))      = " FlatVar " ++ show v ++ " "
  show (Fix (Cut i))      = " Cut " ++ show i ++ " ,"
  show (Fix (Wildcard))   = " Wildcard ," 

fixflatTermPrettyPrint :: Fix FlatTerm -> String
fixflatTermPrettyPrint (Fix (Struct a xs))  = " Struct " ++ a ++ " ["
          ++ Prelude.concat (Prelude.map fixflatTermPrettyPrint xs) 
          ++ "] "
fixflatTermPrettyPrint (Fix (Var v))    = " FlatVar " ++ show v
fixflatTermPrettyPrint (Fix (Cut i))    = " Cut " ++ show i
fixflatTermPrettyPrint (Fix Wildcard)   = " Wildcard "
--}





























{--
variablesOf t = listFilter $ DF.concatMap variableExtractor t

--listFilter :: [FlatTerm t] -> [FlatTerm t]
listFilter []	= []
listFilter (x:xs) = case x of
	(Var v)		-> x : (listFilter xs)
	otherwise	-> listFilter xs

--variableExtractor :: FlatTerm t -> [FlatTerm a]
variableExtractor (Struct a xs) 		= 	subtermEvaluator xs
variableExtractor (Var v)				= 	[Var v]
variableExtractor _						= 	[]

--subtermEvaluator :: [FlatTerm t] -> [FlatTerm a]
subtermEvaluator []					= []
subtermEvaluator (x:xs)				= case x of
	(Var v)				->	(Var v) : subtermEvaluator xs
	(Struct a zs)		->	subtermEvaluator xs ++ zs  
	otherwise			-> 	subtermEvaluator xs

q x = [x]

--translateFlat (Struct a [])		= UTerm		(Struct a [])	
translateFlat (Var v)			= UVar 		(Var v)
translateFlat (Wildcard)		= UTerm 	(Wildcard)	
translateFlat (Cut i)			= UTerm 	(Cut i)
--translateFlat s 				= UTerm		(fmap translateFlat s)

subtermTranslator [] 		= []
subtermTranslator (x:xs)	= case x of
	(Var v)				-> (UVar $ Var v) 								: (subtermTranslator xs)
	(Wildcard)			-> (UTerm $ Wildcard) 							: (subtermTranslator xs)
	(Cut i)				-> (UTerm $ Cut i)								: (subtermTranslator xs)	
--	(Struct a zs) 		-> (UTerm $ Struct a (subtermTranslator zs))	: (subtermTranslator xs)


tf2 t =	case t of 
	UVar _ 	-> t
	UTerm r -> case r of 
		Var v 		-> UVar $ Var v
		Wildcard	-> UTerm $ Wildcard
		Cut i 		-> UTerm $ Cut i
		otherwise 	-> UTerm $ fmap tf2 r

{--
translateToFix (Struct a [])	= Fix (Struct a [])
--translateToFix (Struct a xs)	= Fix (Struct a (fmap translateToFix xs))
translateToFix (Var v) 			= Fix (Var v)
translateToFix (Wildcard)		= Fix (Wildcard)
translateToFix (Cut i)			= Fix (Cut i)
--}

--}

{--
variableNameToDictM :: (Ord a, Unifiable t) =>
    S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
variableNameToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- freeVar
    return $! Map.insert (sv) iv dict
--}


{--
translateToUTerm :: Fix FlatTerm -> UTerm FlatTerm (FlatTerm a)
translateToUTerm (Fix x) = case x of
	Var v				-> UVar $ Var v
	Struct a xs			-> UTerm $ Struct a (Prelude.map translateToUTerm xs)
	Wildcard			-> UTerm Wildcard
	Cut i				-> UTerm $ Cut i
--}
--variableTranslator v varMap = do

{--
translateToUTerm ft = do
  let vs = variableSet ft
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . U.unfreeze ft
  return (t2,varMap)
--}

{--
variableDictionary vSet = foldrM entryInsert Map.empty vSet

--entryInsert :: (Ord k, BindingMonad t a m) => k -> Map k a -> m (Map k (ST.STVar a s))
entryInsert flatVar dict = do
	stVar <- freeVar
	return $! Map.insert flatVar stVar dict

variableTranslator x = do
	x <- newVar (UVar x)
	y <- _newSTVar "a" Nothing	
	z <- freeVar
	return z
--}
