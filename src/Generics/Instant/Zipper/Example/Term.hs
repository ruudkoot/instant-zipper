{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Generics.Instant.Zipper.Example.Term where

import Data.Maybe
import Data.Typeable

import Generics.Instant
import Generics.Instant.TH
import Generics.Instant.Zipper

-- | Datatype

data Term
    =  Var'     String
    |  Lambda   String  Term
    |  App      Term    Term
    |  If       Term    Term Term
    deriving (Eq, Show, Typeable)

-- | Representation

{-
instance Representable Term where
    type Rep Term = (Var String)
                :+: (Var String :*: Rec Term)
                :+: (Rec Term :*: Rec Term)
                :+: (Rec Term :*: Rec Term :*: Rec Term)

    from (Var'  s)      =       L (Var s)
    from (Lambda s t)  =    R (L (Var s :*: Rec t))
    from (App t1 t2)   = R (R (L (Rec t1 :*: Rec t2)))
    from (If t1 t2 t3) = R (R (R (Rec t1 :*: Rec t2 :*: Rec t3)))
    
    to (L (Var s)                               ) = Var' s
    to (R (L (Var s :*: Rec t))                 ) = Lambda s t
    to (R (R (L (Rec t1 :*: Rec t2)))           ) = App t1 t2
    to (R (R (R (Rec t1 :*: Rec t2 :*: Rec t3)))) = If t1 t2 t3
-}

$(deriveAll ''Term)

-- | Family

data TermFam a where
    Term   :: TermFam Term
    String :: TermFam String

deriving instance Show (TermFam a)
    
instance Family TermFam

-- | Zipper

instance Zipper Term
    
-- | fac

fac = Lambda "n"
     (If  (App (App (Var' "=") (Var' "n")) (Var' "0"))
         (Var' "1")
         (App  (App (Var' "+") (Var' "n"))
               (App  (Var' "fac")
                     (App (Var' "pred") (Var' "n")))))


fixFac :: ZipperR Term 
fixFac =  return (enter fac)
      >>= down  Term
      >>= down  Term
      >>= right Term
      >>= right Term
      >>= down  Term
      >>= down  Term
      >>= down  String
      >>= return . set "*"
      >>= return . leave

