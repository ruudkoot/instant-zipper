{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE UndecidableInstances #-}


{-# OPTIONS_GHC -Wall            #-}

module Generics.Instant.Zipper where

import Data.Maybe
import Data.Typeable
--import Debug.Trace

import Control.Applicative
--import Control.Monad

import Generics.Instant

{-
deriving instance (Typeable U)
deriving instance (Typeable2 (:+:))
deriving instance (Typeable2 (:*:))
deriving instance (Typeable1 Rec)
deriving instance (Typeable1 Var)
deriving instance (Typeable2 C)
-}

-- | Utility

impossible :: a
impossible = error "impossible"

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> d) -> (a,b) -> (a,d)
mapSnd f (a,b) = (a, f b)

-- | Basic constructs

data HCtx hole root l where
    Empty :: HCtx hole hole Epsilon
    Next  :: (Zipper parent) => Ctx (Rep parent) -> HCtx parent root cs -> HCtx hole root (parent :<: cs)

data Epsilon = Epsilon deriving Show
data (:<:) c cs = c :<: cs deriving Show

infixr 5 :<:

leave :: (Zipper h) => Loc h r c -> r
leave (Loc h Empty) = h
leave (Loc h (Next c cs)) = leave (Loc (to (fromJust  (fill' c h))) cs)

data Loc h r c = Loc { val :: h, ctxs :: HCtx h r c }

enter :: Zipper h => h -> Loc h h Epsilon
enter h = Loc h Empty

up :: (Zipper h, Zipper h') => Loc h r (h' :<: c) -> Maybe (Loc h' r c)
up (Loc h (Next c cs))   = (\x -> Loc (to x) cs) <$> fill' c h

down :: (Zipper h, Zipper h') => Loc h r c -> Maybe (Loc h' r (h :<: c))
down (Loc h cs) = (\(h', c) -> Loc h' (Next c cs)) <$> first' (from h)

down' :: (Zipper h, Zipper h') => h' -> Loc h r c -> Maybe (Loc h' r (h :<: c))
down' _ (Loc h cs) = (\(h', c) -> Loc h' (Next c cs)) <$> first' (from h)

right :: (Zipper h, Zipper h') => Loc h r cs -> Maybe (Loc h' r cs)
right (Loc _ Empty) = Nothing
right (Loc h (Next c cs)) = (\(h', c') -> Loc h' (Next c' cs)) <$> next' c h

right' :: (Zipper h, Zipper h') => h' -> Loc h r cs -> Maybe (Loc h' r cs)
right' _ (Loc _ Empty) = Nothing
right' _ (Loc h (Next c cs)) = (\(h', c') -> Loc h' (Next c' cs)) <$> next' c h

-- | Zipper

class ( Representable  f
      , Typeable       f
      , Fillable  (Rep f)
      , Firstable (Rep f)
      , Nextable  (Rep f) ) => Zipper f

instance Zipper Int
instance Zipper Char
instance Zipper Float

-- | Zippable

class Zippable f where
    data Ctx f :: * 
    
instance Zippable Int where
    data Ctx Int
    
instance Zippable U where
    data Ctx U
    
instance (Zippable f, Zippable g) => Zippable (f :+: g) where
    data Ctx (f :+: g) = CL (Ctx f) | CR (Ctx g)
    
instance (Zippable f, Zippable g) => Zippable (f :*: g) where
    data Ctx (f :*: g) = C1 (Ctx f) g | C2 f (Ctx g)
    
instance (Zippable a) => Zippable (Rec a) where
    data Ctx (Rec a) = Recursive
    
instance (Zippable a) => Zippable (Var a) where
    data Ctx (Var a) = Variable
    
instance (Zippable f) => Zippable (C c f) where
    data Ctx (C c f) = CC (Ctx f)

-- | Fill

class Fillable f where
    fill' :: (Typeable a) => Ctx f -> a -> Maybe f

instance Fillable U where
    fill' = impossible

instance Fillable Char where
    fill' = impossible

instance Fillable Int where
    fill' = impossible

instance Fillable Float where
    fill' = impossible
            
instance (Fillable f, Fillable g) => Fillable (f :+: g) where
--    fill :: Ctx (f :+: g) -> a -> (f :+: g)
    fill' (CL l) v = L <$> fill' l v
    fill' (CR r) v = R <$> fill' r v

instance (Fillable f, Fillable g) => Fillable (f :*: g) where
--    fill :: Ctx (f :+: g) -> a -> (f :+: g)
    fill' (C1 c r) v = flip (:*:) r <$> fill' c v
    fill' (C2 l c) v = (l :*:) <$> fill' c v

instance (Typeable a) => Fillable (Rec a) where
    fill' Recursive v = Rec <$> cast v
    
instance (Typeable a) => Fillable (Var a) where
    fill' Variable v = Var <$> cast v

instance (Fillable f) => Fillable (C c f) where
    fill' (CC c) v = C <$> fill' c v
    
   
-- | First

class Firstable f where
    first' :: (Zipper a) => f -> Maybe (a, Ctx f)

instance Firstable U where
    first' _ = Nothing

instance Firstable Char where
    first' _ = Nothing -- impossible?

instance Firstable Int where
    first' _ = Nothing -- impossible?
    
instance Firstable Float where
    first' _ = Nothing -- impossible?
            
instance (Firstable f, Firstable g) => Firstable (f :+: g) where
    first' (L x) = mapSnd CL <$> first' x
    first' (R y) = mapSnd CR <$> first' y

instance (Firstable f, Firstable g) => Firstable (f :*: g) where
    first' (l :*: r) = mapSnd (flip C1 r) <$> first' l
                   <|> mapSnd (C2 l) <$> first' r

instance (Typeable f) => Firstable (Rec f) where
    first' (Rec v) = (\x -> (x, Recursive)) <$> cast v

instance (Typeable f) => Firstable (Var f) where
    first' (Var v) = (\x -> (x, Variable)) <$> cast v

instance (Firstable f) => Firstable (C c f) where
    first' (C v) = mapSnd CC <$> first' v
    
-- | Next

class Nextable f where
    next' :: (Typeable a, Zipper b) => Ctx f -> a -> Maybe (b, Ctx f)
    
instance Nextable U where
    next' _ _ = Nothing

instance Nextable Char where
    next' _ _ = Nothing

instance Nextable Int where
    next' _ _ = Nothing

instance Nextable Float where
    next' _ _ = Nothing

instance (Nextable f, Nextable g) => Nextable (f :+: g) where
    next' (CL c) x = mapSnd CL <$> next' c x
    next' (CR c) y = mapSnd CR <$> next' c y

instance (Nextable f, Nextable g, Fillable f, Firstable g) => Nextable (f :*: g) where
    next' (C1 c y) x = mapSnd (flip C1 y) <$> next' c x
                    <|> (\x' (y',c') -> (y', C2 x' c')) <$> fill' c x <*> first' y
    next' (C2 x c) y = mapSnd (C2 x) <$> next' c y 

instance Nextable (Rec f) where
    next' (Recursive) _ = Nothing

instance Nextable (Var f) where
    next' (Variable) _ = Nothing

instance (Nextable f) => Nextable (C c f) where
    next' (CC v) x = mapSnd CC <$> next' v x

-- | Type-level functions
