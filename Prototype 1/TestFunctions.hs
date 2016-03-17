module TestFunctions where

import Data.Functor.Fixedpoint   as DFF
import Control.Unification.STVar as ST
import Data.Map                  as Map
import Control.Unification       as U
import Control.Unification.Types as UT
import Control.Applicative ((<$>),(<*>),pure,Applicative)
import Control.Monad.Error
import PrologLanguage
import Translators

instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}

test1 ::
  ErrorT
  (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
  (ST.STBinding s)
  (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
   Map VariableName (ST.STVar s (FlatTerm)))
test1 = do
    let
        t1a = (Fix $ Var $ VariableName 0 "x")
        t2a = (Fix $ Var $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)

test2 ::
  ErrorT
  (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
  (ST.STBinding s)
  (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
   Map VariableName (ST.STVar s (FlatTerm)))
test2 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Var $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)


test3 ::
  ErrorT
  (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
  (ST.STBinding s)
  (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
   Map VariableName (ST.STVar s (FlatTerm)))
test3 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Var $ VariableName 0 "x")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)
{--
goTest test3
"ok:    STVar -9223372036854775807
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

test4 ::
  ErrorT
  (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
  (ST.STBinding s)
  (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
   Map VariableName (ST.STVar s (FlatTerm)))
test4 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Var $ VariableName 0 "x")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unifyOccurs x1 x2
    return (x3, d1 `Map.union` d2)
{--
goTest test4
"ok:    STVar -9223372036854775807
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

test5 ::
  ErrorT
  (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
  (ST.STBinding s)
  (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
   Map VariableName (ST.STVar s (FlatTerm)))
test5 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Struct "b" [Fix $ Var $ VariableName 0 "y"])
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)


test6 ::
  ErrorT
  (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
  (ST.STBinding s)
  (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
   Map VariableName (ST.STVar s (FlatTerm)))
test6 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "y"])
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)
