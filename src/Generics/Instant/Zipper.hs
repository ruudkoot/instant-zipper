{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -Wall           #-}

module Generics.Instant.Zipper (
    module Data.Typeable,
    module Generics.Instant,
    -- *
    Family,
    Zipper,
    -- *
    enter,
    leave,
    up,
    down,
    downR,
    left,
    right,
    get,
    set,
    -- *
    PrimFam(..)
) where

import Prelude hiding (last)

import Control.Applicative

import Data.Maybe
import Data.Typeable

import Generics.Instant


-- | Type-level list

data Epsilon
data c :<: cs

infixr 5 :<:

-- | Loc and Context

data Loc hole root c = (:|:) { focus :: hole, context :: Context hole root c }

data Context hole root l where
    Empty :: Context hole hole Epsilon
    (:-)  :: (Zipper parent) => Derivative (Rep parent)
                             -> Context parent root cs
                             -> Context hole root (parent :<: cs)
                             
infix 6 :|:
infix 7 :-

-- | Families

class Family (f :: * -> *)

data PrimFam a where
    Char  ::                                  PrimFam Char
    Int   ::                                  PrimFam Int
    Float ::                                  PrimFam Float
    List  :: (Family f, Show (f a)) => f a -> PrimFam [a]
    
deriving instance Show (PrimFam a)

instance Family PrimFam

-- | Monadic cast

maybeToMonad :: (Monad m) => String -> Maybe a -> m a
maybeToMonad e = maybe (fail e) return

castM :: (Typeable a, Typeable b, Monad m) => a -> m b
castM = maybeToMonad "cannot cast" . cast

-- | Zipper

class ( Representable  f
      , Typeable       f
      , Fillable  (Rep f)
      , Firstable (Rep f)
      , Nextable  (Rep f)
      , Lastable  (Rep f)
      , Prevable  (Rep f) ) => Zipper f

instance Zipper Int
instance Zipper Char
instance Zipper Float
instance (Zipper a) => Zipper [a]

-- | Differentiable

class Differentiable f where
    data Derivative f :: * 
    
instance Differentiable U where
    data Derivative U
    
instance (Differentiable f, Differentiable g) => Differentiable (f :+: g) where
    data Derivative (f :+: g) = CL (Derivative f)
                              | CR (Derivative g)
    
instance (Differentiable f, Differentiable g) => Differentiable (f :*: g) where
    data Derivative (f :*: g) = C1 (Derivative f) g
                              | C2 f (Derivative g)
    
instance (Differentiable a) => Differentiable (Rec a) where
    data Derivative (Rec a) = CRec
    
instance (Differentiable a) => Differentiable (Var a) where
    data Derivative (Var a) = CVar
    
instance (Differentiable f) => Differentiable (C c f) where
    data Derivative (C c f) = CC (Derivative f)

-- | Fill

class Fillable f where
    fill :: (Typeable a, Monad m, Applicative m) => Derivative f -> a -> m f

instance Fillable U where
    fill = error "impossible"

instance Fillable Char where
    fill = error "impossible"

instance Fillable Int where
    fill = error "impossible"

instance Fillable Float where
    fill = error "impossible"
            
instance (Fillable f, Fillable g) => Fillable (f :+: g) where
    fill (CL l) v = L <$> fill l v
    fill (CR r) v = R <$> fill r v

instance (Fillable f, Fillable g) => Fillable (f :*: g) where
    fill (C1 c r) v = flip (:*:) r <$> fill c v
    fill (C2 l c) v = (l :*:) <$> fill c v

instance (Typeable a) => Fillable (Rec a) where
    fill CRec v = Rec <$> castM v
    
instance (Typeable a) => Fillable (Var a) where
    fill CVar v = Var <$> castM v

instance (Fillable f) => Fillable (C c f) where
    fill (CC c) v = C <$> fill c v
    
   
-- | First

class Firstable f where
    first :: (Zipper a, Monad m, Alternative m) => f -> m (a, Derivative f)

instance Firstable U where
    first _ = fail "first U"

instance Firstable Char where
    first _ = fail "first Char"

instance Firstable Int where
    first _ = fail "first Int"
    
instance Firstable Float where
    first _ = fail "first Float"
            
instance (Firstable f, Firstable g) => Firstable (f :+: g) where
    first (L x)     = fmap CL <$> first x
    first (R y)     = fmap CR <$> first y

instance (Firstable f, Firstable g) => Firstable (f :*: g) where
    first (l :*: r) =  fmap (flip C1 r) <$> first l
                   <|> fmap (     C2 l) <$> first r

instance (Typeable f) => Firstable (Rec f) where
    first (Rec v) = (, CRec) <$> castM v

instance (Typeable f) => Firstable (Var f) where
    first (Var v) = (, CVar) <$> castM v

instance (Firstable f) => Firstable (C c f) where
    first (C v)   = fmap CC <$> first v

-- | Last

class Lastable f where
    last :: (Zipper a, Monad m, Alternative m) => f -> m (a, Derivative f)

instance Lastable U where
    last _ = fail "last U"

instance Lastable Char where
    last _ = fail "last Char"

instance Lastable Int where
    last _ = fail "last Int"
    
instance Lastable Float where
    last _ = fail "last Float"
            
instance (Lastable f, Lastable g) => Lastable (f :+: g) where
    last (L x) = fmap CL <$> last x
    last (R y) = fmap CR <$> last y

instance (Lastable f, Lastable g) => Lastable (f :*: g) where
    last (l :*: r) =  fmap (     C2 l) <$> last r
                  <|> fmap (flip C1 r) <$> last l

instance (Typeable f) => Lastable (Rec f) where
    last (Rec v) = (, CRec) <$> castM v

instance (Typeable f) => Lastable (Var f) where
    last (Var v) = (, CVar) <$> castM v

instance (Lastable f) => Lastable (C c f) where
    last (C v) = fmap CC <$> last v
    
-- | Next

class Nextable f where
    next :: (Typeable a, Zipper b, Monad m, Alternative m) =>
                Derivative f -> a -> m (b, Derivative f)
    
instance Nextable U where
    next _ _ = fail "next U"

instance Nextable Char where
    next _ _ = fail "next Char"

instance Nextable Int where
    next _ _ = fail "next Int"

instance Nextable Float where
    next _ _ = fail "next Float"

instance (Nextable f, Nextable g) => Nextable (f :+: g) where
    next (CL c) x = fmap CL <$> next c x
    next (CR c) y = fmap CR <$> next c y

instance (Nextable f, Nextable g, Fillable f, Firstable g) => Nextable (f :*: g) where
    next (C1 c y) x = fmap (flip C1 y) <$> next c x
                   <|> (\x' (y',c') -> (y', C2 x' c')) <$> fill c x <*> first y
    next (C2 x c) y = fmap (C2 x) <$> next c y 

instance Nextable (Rec f) where
    next CRec _ = fail "next CRec"

instance Nextable (Var f) where
    next CVar _ = fail "next CVar"

instance (Nextable f) => Nextable (C c f) where
    next (CC v) x = fmap CC <$> next v x

-- | Prev

class Prevable f where
    prev :: (Typeable a, Zipper b, Monad m, Alternative m) =>
                Derivative f -> a -> m (b, Derivative f)
    
instance Prevable U where
    prev _ _ = fail "prev U"

instance Prevable Char where
    prev _ _ = fail "prev Char"

instance Prevable Int where
    prev _ _ = fail "prev Int"

instance Prevable Float where
    prev _ _ = fail "prev Float"

instance (Prevable f, Prevable g) => Prevable (f :+: g) where
    prev (CL c) x = fmap CL <$> prev c x
    prev (CR c) y = fmap CR <$> prev c y

instance (Lastable f, Fillable g, Prevable f, Prevable g) => Prevable (f :*: g) where
    prev (C1 c y) x = fmap (flip C1 y) <$> prev c x                    
    prev (C2 x c) y = fmap (     C2 x) <$> prev c y 
                   <|> (\x' (y',c') -> (y', C1 c' x')) <$> fill c y <*> last x

instance Prevable (Rec f) where
    prev CRec _ = fail "prev CRec"

instance Prevable (Var f) where
    prev CVar _ = fail "prev CVar"

instance (Prevable f) => Prevable (C c f) where
    prev (CC v) x = fmap CC <$> prev v x

-- | Navigation

get :: Loc h r c -> h
get = focus

set :: h -> Loc h r c -> Loc h r c
set v (_ :|: cs) = v :|: cs

enter :: Zipper h => h -> Loc h h Epsilon
enter h = h :|: Empty

leave :: (Zipper h) => Loc h r c -> r
leave     (h :|: Empty  ) = h
leave loc@(_ :|: _ :- _) = leave (up loc)

-- | Up

up :: (Zipper h, Zipper h') => Loc h r (h' :<: c) -> Loc h' r c
up (h :|: c :- cs) = fromJust $ do x <- fill c h
                                   return (to x :|: cs)

-- | Down (left)

down :: (Family f, Zipper h, Zipper h', Monad m, Alternative m)
     => f h' -> Loc h r c -> m (Loc h' r (h :<: c))
down _ (h :|: cs) = do (h', c) <- first (from h) 
                       return (h' :|: c :- cs)

-- | Down right

downR :: (Family f, Zipper h, Zipper h', Monad m, Alternative m)
      => f h' -> Loc h r c -> m (Loc h' r (h :<: c))
downR _ (h :|: cs) = do (h', c) <- last (from h) 
                        return (h' :|: c :- cs)

-- | Right

right :: (Family f, Zipper h, Zipper h', Monad m, Alternative m)
      => f h' -> Loc h r (c :<: cs) -> m (Loc h' r (c :<: cs))
right _ (h :|: c :- cs) = do (h', c') <- next c h
                             return (h' :|: c' :- cs)

-- | Left

left :: (Family f, Zipper h, Zipper h', Monad m, Alternative m)
     => f h' -> Loc h r (c :<: cs) -> m (Loc h' r (c :<: cs))
left _ (h :|: c :- cs) = do (h', c') <- prev c h
                            return (h' :|: c' :- cs)

