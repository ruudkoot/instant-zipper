{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Generics.Instant.Zipper where

import Data.Maybe
import Data.Typeable
import Generics.Instant

deriving instance (Typeable U)
deriving instance (Typeable2 (:+:))
deriving instance (Typeable2 (:*:))
deriving instance (Typeable1 Rec)
deriving instance (Typeable1 Var)
deriving instance (Typeable2 C)

impossible = error "impossible"

{-
class Zipper f where
    data Ctx f :: * -> *
    cmapA       :: (r -> Maybe r') -> Ctx f r -> Maybe (Ctx f r')
    fill        :: Ctx f r -> r -> f r
    first, last :: (r -> Ctx f r -> a) -> f r -> Maybe a
    next, prev  :: (r -> Ctx f r -> a) -> Ctx f r -> r -> Maybe a
-}

class (Representable f, Typeable f, Fillable f, Firstable f) => Zipper f where
    data Ctx f :: *
    --cmapA       :: (r -> Maybe r') -> Ctx f r -> Maybe (Ctx f r')

    first, last :: (f -> Ctx f -> a) -> f -> Maybe a
    next, prev  :: (f -> Ctx f -> a) -> Ctx f -> f -> Maybe a


instance Zipper U where
    data Ctx U
    --fill ctx f = impossible
    first _ U  = Nothing
    last  _ U  = Nothing
    next  _ _ _ = impossible
    prev  _ _ _ = impossible
    
instance (Zipper f, Zipper g) => Zipper (f :+: g) where
    data Ctx (f :+: g) = CL (Ctx f) | CR (Ctx g)

    --fill (CL c) x = L (fill c x)
    --fill (CR c) y = R (fill c y)
    
    --first f (L x) = first (\z -> f z . CL) x
    
    
instance (Zipper f, Zipper g) => Zipper (f :*: g) where
    data Ctx (f :*: g) = C1 (Ctx f) g | C2 f (Ctx g)
    
instance (Zipper a) => Zipper (Rec a) where
    data Ctx (Rec a) = Recursive
    
instance (Zipper a) => Zipper (Var a) where
    data Ctx (Var a) = Variable
    
instance (Zipper f, Typeable c) => Zipper (C c f) where
    data Ctx (C c f) = CC (Ctx f)

-- | Fill

class Fillable f where
    fill' :: (Typeable a) => Ctx f -> a -> f
    
instance (Fillable f, Fillable g) => Fillable (f :+: g) where
--    fill :: Ctx (f :+: g) -> a -> (f :+: g)
    fill' (CL l) v = L (fill' l v) 
    fill' (CR r) v = R (fill' r v) 

instance (Fillable f, Fillable g) => Fillable (f :*: g) where
--    fill :: Ctx (f :+: g) -> a -> (f :+: g)
    fill' (C1 c r) v = (fill' c v) :*: r 
    fill' (C2 l c) v = l :*: (fill' c v) 

instance (Zipper a) => Fillable (Rec a) where
    fill' (Recursive) v = fromJust $ cast v
{-    
fill :: (Rewritable f, Rewritable f') => Ctx f -> f -> f'
fill = 

class Fillable f where
    fill        :: Ctx f -> f -> f
-}


-- | First

--first = undefined

class Firstable f where
    first' :: (Zipper f') => (f' -> Ctx f' -> a) -> f -> Maybe a
    
instance (Firstable f, Firstable g) => Firstable (f :+: g) where
    --first' :: 
    first' f (L x) = let cont z = f z . CL
                     in first' cont x
    --first' f (R y) = first' (\z -> f z . CR) y


