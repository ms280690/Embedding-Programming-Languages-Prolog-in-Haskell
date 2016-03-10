module Translators where

import Data.Functor.Fixedpoint as DFF

import Control.Unification.STVar as ST 

import Data.Map as Map

import Control.Unification as U

import Control.Unification.Types as UT

import Control.Applicative ((<$>),(<*>),pure,Applicative)

import PrologLanguage

import VariableHandler
{--
uTermify :: Ord a =>
            Map a (ST.STVar s (FlatTerm)) ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)) ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm))
--}
uTermify 
  :: Map VariableName (ST.STVar s (FlatTerm)) 
  -> UTerm FlatTerm (ST.STVar s (FlatTerm)) 
  -> UTerm FlatTerm (ST.STVar s (FlatTerm))
uTermify varMap ux = case ux of
  UT.UVar _          		-> ux
  UT.UTerm (Var v)  		-> maybe (error "bad map") UT.UVar $ Map.lookup v varMap
 -- UT.UTerm t         		-> UT.UTerm $! fmap (uTermify varMap) 
  UT.UTerm (Struct a xs)	-> UT.UTerm $ Struct a $! fmap (uTermify varMap) xs  	
  UT.UTerm (Wildcard)		-> UT.UTerm Wildcard
  UT.UTerm (Cut i)			-> UT.UTerm (Cut i)

translateToUTerm ::
    Fix FlatTerm -> ST.STBinding s
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
translateToUTerm e1Term = do
  let vs = variableNameSet $ variableNameExtractor e1Term
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . unfreeze $ e1Term
  return (t2,varMap)


-- | vTermify recursively converts @UVar x@ into @UTerm (VarA x).
-- This is a subroutine of @ translateFromUTerm @.  The resulting
-- term has no (UVar x) subterms.

vTermify :: Map Int VariableName ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)) ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm))
vTermify dict t1 = case t1 of
  UT.UVar x  -> maybe (error "logic") (UT.UTerm . Var) $ Map.lookup (UT.getVarID x) dict
  UT.UTerm r ->
    case r of
      Var iv   -> t1
      _         -> UT.UTerm . fmap (vTermify dict) $ r

translateFromUTerm :: 
    Map VariableName (ST.STVar s (FlatTerm)) ->
    UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)) -> Prolog
translateFromUTerm dict uTerm =
  P .  maybe (error "Logic") id . freeze . vTermify varIdDict $ uTerm where
    forKV dict initial fn = Map.foldlWithKey' (\a k v -> fn k v a) initial dict
    varIdDict = forKV dict Map.empty $ \ k v -> Map.insert (UT.getVarID v) k

mapWithKeyM :: (Ord k,Applicative m,Monad m)
               => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM = Map.traverseWithKey


makeDict :: 
            Map VariableName (ST.STVar s (FlatTerm)) -> ST.STBinding s (Map VariableName (Prolog))
makeDict sVarDict =
    flip mapWithKeyM sVarDict $ \ _ -> \ iKey -> do
        Just xx <- UT.lookupVar $ iKey
        return $! (translateFromUTerm sVarDict) xx