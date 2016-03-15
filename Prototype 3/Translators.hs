module Translators where

import Data.Functor.Fixedpoint as DFF

import Control.Unification.STVar as ST 

import Data.Map as Map

import Control.Unification as U

import Control.Unification.Types as UT

import Control.Applicative ((<$>),(<*>),pure,Applicative)

import Control.Monad.Trans.Except

import Control.Monad.Error

import PrologFlat

import VariableHandler

import Prolog


uTermify 
  :: Map Id (ST.STVar s (FTS)) 
  -> UTerm FTS (ST.STVar s (FTS)) 
  -> UTerm FTS (ST.STVar s (FTS))
uTermify varMap ux = case ux of
  UT.UVar _             -> ux
  UT.UTerm (FV v)       -> maybe (error "bad map") UT.UVar $ Map.lookup v varMap
 -- UT.UTerm t            -> UT.UTerm $! fmap (uTermify varMap) t
  UT.UTerm (FS a xs)    -> UT.UTerm $ FS a $! fmap (uTermify varMap) xs   


translateToUTerm ::
    Fix FTS -> ST.STBinding s
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS)))
translateToUTerm e1Term = do
  let vs = variableNameSet $ variableIdExtractor e1Term
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . unfreeze $ e1Term
  return (t2,varMap)


-- | vTermify recursively converts @UVar x@ into @UTerm (VarA x).
-- This is a subroutine of @ translateFromUTerm @.  The resulting
-- term has no (UVar x) subterms.

vTermify :: Map Int Id ->
            UT.UTerm (FTS) (ST.STVar s (FTS)) ->
            UT.UTerm (FTS) (ST.STVar s (FTS))
vTermify dict t1 = case t1 of
  UT.UVar x  -> maybe (error "logic") (UT.UTerm . FV) $ Map.lookup (UT.getVarID x) dict
  UT.UTerm r ->
    case r of
      FV iv   -> t1
      _       -> UT.UTerm . fmap (vTermify dict) $ r

translateFromUTerm :: 
    Map Id (ST.STVar s (FTS)) ->
    UT.UTerm (FTS) (ST.STVar s (FTS)) -> Prolog
translateFromUTerm dict uTerm =
  P .  maybe (error "Logic") id . freeze . vTermify varIdDict $ uTerm where
    forKV dict initial fn = Map.foldlWithKey' (\a k v -> fn k v a) initial dict
    varIdDict = forKV dict Map.empty $ \ k v -> Map.insert (UT.getVarID v) k


mapWithKeyM :: (Ord k,Applicative m,Monad m)
               => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM = Map.traverseWithKey


makeDict :: 
            Map Id (ST.STVar s (FTS)) -> ST.STBinding s (Map Id (Prolog))
makeDict sVarDict =
    flip mapWithKeyM sVarDict $ \ _ -> \ iKey -> do
        Just xx <- UT.lookupVar $ iKey
        return $! (translateFromUTerm sVarDict) xx


-- | recover the bindings for the variables of the two terms
-- unified from the monad.

makeDicts :: 
    (Map Id (ST.STVar s (FTS)), Map Id (ST.STVar s (FTS))) ->
    ExceptT  (UT.UFailure (FTS) (ST.STVar s (FTS)))
    (ST.STBinding s) (Map Id (Prolog))
makeDicts (svDict1, svDict2) = do
  let svDict3 = (svDict1 `Map.union` svDict2)
  let ivs = Prelude.map UT.UVar . Map.elems $ svDict3
  applyBindingsAll ivs
  -- the interface below is dangerous because Map.union is left-biased.
  -- variables that are duplicated across terms may have different
  -- bindings because `translateToUTerm` is run separately on each
  -- term.
  lift . makeDict $ svDict3