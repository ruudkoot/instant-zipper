{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Generics.Instant.Zipper.Test.Term where

import Data.Maybe
import Generics.Instant.Zipper

-- | Datatype

data Term
    =  Var'     String
    |  Lambda   String  Term
    |  App      Term    Term
    |  If       Term    Term Term
    deriving (Eq, Show, Typeable)

-- | Representation

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

-- | Zipper

instance Zipper Term
    
-- | fac

fac = Lambda "n"
     (If  (App (App (Var' "=") (Var' "n")) (Var' "0"))
         (Var' "1")
         (App  (App (Var' "+") (Var' "n"))
               (App  (Var' "fac")
                     (App (Var' "pred") (Var' "n")))))

--fixFac :: (Zipper f') => Maybe (Loc f' (Ctx (Rep Term) :<: Epsilon))
fixFac :: Maybe (Loc Term (Ctx (Rep Term) :<: Epsilon))
fixFac =  return (enter fac)
            >>= down
--          >>= down
--          >>= right
--          >>= right
--          >>= down
--          >>= down
--          >>= return . setHole_Term (Var "*")
--          >>= return . fromZipper_Term

t0 :: Loc Term Epsilon
t0 = enter fac

t1 :: Loc Term (Ctx (Rep Term) :<: Epsilon)
t1 = fromJust $ down t0

t2 :: Loc Term (Ctx (Rep Term) :<: Ctx (Rep Term) :<: Epsilon)
t2 = fromJust $ down t1
