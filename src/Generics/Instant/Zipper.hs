{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}

{-# OPTIONS_GHC -Wall            #-}

module Generics.Instant.Zipper
( module Data.Typeable,
  module Generics.Instant,
  -- *
  Zipper,
  Ctx,
  Loc,
  Epsilon,
  (:<:),
  -- *
  enter,
  down,
  up
) where

--import Data.Maybe
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

data Epsilon    = Epsilon                     deriving (Show)
data (:<:) c cs = c :<: cs                    deriving (Show)

infixr 5 :<:

data Loc h c = Loc { hole :: h, context :: c }    deriving (Show)

enter :: Zipper f => f -> Loc f Epsilon
enter f = Loc f Epsilon

{-
leave :: Zipper f => Loc f -> f
leave (Loc f []) = f
leave loc = leave (fromMaybe (error "Error leaving") (up loc))
-}

up :: (Zipper f, Zipper f') => Loc f (Ctx (Rep f') :<: c) -> Maybe (Loc f' c)
-- up (Log f Epsilon) ?
up (Loc f (c :<: cs))   = (\x -> Loc (to x) cs) <$> fill' c f

down :: (Zipper f, Zipper f') => Loc f c -> Maybe (Loc f' (Ctx (Rep f) :<: c))
down (Loc f cs) = (\(f', c) -> Loc f' (c :<: cs)) <$> first' (from f)


--        where wrap :: (f', Ctx (Rep f)) -> Loc f'
--              wrap (f', c) = Loc f' (Push (fromJust . gcast $ c) cs)

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

-- Requires GHC 7 :p
deriving instance (Show (Ctx Int))
deriving instance (Show (Ctx U))
deriving instance (Show (Ctx f), Show (Ctx g)) => (Show (Ctx (f :+: g)))
deriving instance (Show f, Show g, Show (Ctx f), Show (Ctx g)) => (Show (Ctx (f :*: g)))
deriving instance (Show (Ctx (Rec a)))
deriving instance (Show (Ctx (Var a)))
deriving instance (Show (Ctx f)) => (Show (Ctx (C c f)))

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

