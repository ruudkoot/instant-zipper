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

--data HList l where
--    Empty :: HList Epsilon
--    Next  :: a -> HList cs -> HList (a :<: cs)

data HCtx hole root l where
    Empty :: HCtx hole hole Epsilon
    Next  :: Ctx (Rep parent) -> HCtx parent root cs -> HCtx hole root (parent :<: cs)

{-
data Ctxs h r where
    Empty :: Ctxs r r
    Add   :: Ctx (Rep c) -> Ctxs p r -> Ctxs c r
-}    
data Epsilon = Epsilon
data (:<:) c cs = c :<: cs

{-
length' :: HList l -> Int
length' Empty = 0
length' (Next e cs) = 1 + length' cs

head' :: HList (a :<: cs) -> a
head' (Next e cs) = e
-}


leave :: (Zipper h) => Loc h r c -> r
leave (Loc h Empty) = h
leave (Loc h (Next c cs)) = leave (Loc (to (fromJust  (fill' c h))) cs)

data Loc h r c = Loc { val :: h, ctxs :: HCtx h r c }

enter :: Zipper f => f -> Loc f f Epsilon
enter f = Loc f Empty

{-
leave :: Zipper f => Loc f -> f
leave (Loc f []) = f
leave loc = leave (fromMaybe (error "Error leaving") (up loc))
-}

up :: (Zipper h, Zipper h') => Loc h r (h' :<: c) -> Maybe (Loc h' r c)
up (Loc h (Next c cs))   = (\x -> Loc (to x) cs) <$> fill' c h

{-
down :: (Zipper f, Zipper f') => Loc f c -> Maybe (Loc f' (Ctx (Rep f) :<: c))
down (Loc f cs) = (\(f', c) -> Loc f' (c :<: cs)) <$> first' (from f)
-}

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

