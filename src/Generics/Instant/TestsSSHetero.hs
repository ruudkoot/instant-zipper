{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}

module Generics.Instant.TestsSSHetero where

import Generics.Instant
import Generics.Instant.TH
import Generics.Instant.Zipper
import Generics.Instant.Rewriting

import Data.Maybe
import Data.Typeable
import Debug.Trace

data Exp = Const Int | Plus Exp Exp deriving (Typeable,Show)

 -- Auxiliary datatypes for constructor representations
data Const
data Plus

instance Constructor Const where conName _ = "Const"
instance Constructor Plus  where conName _ = "Plus"

-- Representable instance
instance Representable Exp where
    type Rep Exp = (Var Int) :+: (Rec Exp :*: Rec Exp)

    from (Const n)   = L (Var n)
    from (Plus e e') = R (Rec e :*: Rec e')

    to (L (Var n))            = Const n
    to (R (Rec e :*: Rec e')) = Plus e e'

testExp1 = Plus (Plus (Const 0) (Const 1)) (Plus (Const 2) (Const 3))

instance Zipper Exp where



firstHole :: Exp -> Exp
firstHole = fst . fromJust . first' . from

test :: Exp
test = firstHole testExp1

downExp :: Loc Exp c -> Maybe (Loc Exp (HCons (Ctx (Rep Exp)) c))
downExp = down

fContext :: Loc Exp (HCons (Ctx (Rep Exp)) HNil)
fContext = fromJust . down . enter $ testExp1

test2 :: Exp
test2 = val fContext

test3 :: Exp
test3 = val . fromJust . up $ fContext


--test4 :: Exp
--test4 = to . fromJust . (\(Loc f (Push c cs)) -> fill' c f) $ fContext
{-
test3 :: Exp
test3 = leave . fromJust . down . enter $ testExp1
-}
