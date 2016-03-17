module TestFunctions where


import Control.Applicative ((<$>),(<*>),pure,Applicative)
import Control.Monad.Error
import Control.Unification as U
import Control.Unification.STVar as ST
import Control.Unification.Types as UT
import Data.Functor.Fixedpoint as DFF
import Data.Map as Map
import Prolog
import PrologFlat
import Translators

instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}

test1 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS)))
test1 = do
    let
        t1a = (Fix $ FV $ (0, "x"))
        t2a = (Fix $ FV $ (1, "y"))
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)


test2 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS)))
test2 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ (0, "x")])
        t2a = (Fix $ FV $ (1, "y"))
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)


test3 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS)))
test3 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ (0, "x")])
        t2a = (Fix $ FV $ (0, "x"))
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)
{--
goTest test3
"ok:    STVar -9223372036854775807
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

test4 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS)))
test4 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ (0, "x")])
        t2a = (Fix $ FV $ (0, "x"))
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unifyOccurs x1 x2
    return (x3, d1 `Map.union` d2)
{--
goTest test4
"ok:    STVar -9223372036854775807
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

test5 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS)))
test5 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ (0, "x")])
        t2a = (Fix $ FS "b" [Fix $ FV $ (0, "y")])
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)
