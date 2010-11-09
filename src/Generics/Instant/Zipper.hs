{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wall            #-}

module Generics.Instant.Zipper where

import Control.Applicative

import Data.Maybe
import Data.Typeable

import Generics.Instant

-- | Utility

impossible :: a
impossible = error "impossible"

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> d) -> (a,b) -> (a,d)
mapSnd f (a,b) = (a, f b)

-- | Basic constructs

class Family (f :: * -> *) where

data Epsilon = Epsilon deriving Show
data (:<:) c cs = c :<: cs deriving Show

infixr 5 :<:

data Loc hole root c = Loc { focus :: hole, context :: Context hole root c }

data Context hole root l where
    Empty :: Context hole hole Epsilon
    Push  :: (Zipper parent) => Derivative (Rep parent)
                             -> Context parent root cs
                             -> Context hole root (parent :<: cs)

type ZipperR = Either String

-- | Navigation

getHole :: Loc h r c -> h
getHole = focus

setHole :: h -> Loc h r c -> Loc h r c
setHole v (Loc _ cs) = Loc v cs

leave :: (Zipper h) => Loc h r c -> r
leave (Loc h Empty) = h
leave (Loc h (Push c cs)) = leave (Loc (to (fromJust  (fill' c h))) cs)

enter :: Zipper h => h -> Loc h h Epsilon
enter h = Loc h Empty


up :: (Zipper h, Zipper h') => Loc h r (h' :<: c) -> Loc h' r c
up (Loc h (Push c cs))   = fromJust $ (\x -> Loc (to x) cs) <$> fill' c h

-- | Down left

downL_ :: (Zipper h, Zipper h') => Loc h r c -> ZipperR (Loc h' r (h :<: c))
downL_ (Loc h cs) = maybe (Left "Error going down left") (\(h', c) -> Right (Loc h' (Push c cs))) $ first' (from h)

downL' :: (Zipper h, Zipper h') => h' -> Loc h r c -> ZipperR (Loc h' r (h :<: c))
downL' _ = downL_

downL :: (Zipper h, Zipper h', Family f, Show (f h')) => f h' -> Loc h r c -> ZipperR (Loc h' r (h :<: c))
downL v = either (Left . (++ " with type " ++ show v)) Right . downL_

-- | Down 

down_ :: (Zipper h, Zipper h') => Loc h r c -> ZipperR (Loc h' r (h :<: c))
down_ = downL_

down' :: (Zipper h, Zipper h') => h' -> Loc h r c -> ZipperR (Loc h' r (h :<: c))
down' = downL'

down :: (Zipper h, Zipper h', Family f, Show (f h')) => f h' -> Loc h r c -> ZipperR (Loc h' r (h :<: c))
down = downL

-- | Down Right

downR_ :: (Zipper h, Zipper h') => Loc h r c -> ZipperR (Loc h' r (h :<: c))
downR_ (Loc h cs) = maybe (Left "Error going down right") (\(h', c) -> Right (Loc h' (Push c cs))) $ last' (from h)

downR' :: (Zipper h, Zipper h') => h' -> Loc h r c -> ZipperR (Loc h' r (h :<: c))
downR' _ = downR_ 

downR :: (Zipper h, Zipper h', Family f, Show (f h')) => f h' -> Loc h r c -> ZipperR (Loc h' r (h :<: c))
downR v = either (Left . (++ " with type " ++ show v)) Right . downR_

-- | Right

right_ :: (Zipper h, Zipper h') => Loc h r (c :<: cs) -> ZipperR (Loc h' r (c :<: cs))
right_ (Loc h (Push c cs)) = maybe (Left "Error going right") (\(h', c') -> Right (Loc h' (Push c' cs))) $ next' c h

right' :: (Zipper h, Zipper h') => h' -> Loc h r (c :<: cs) -> ZipperR (Loc h' r (c :<: cs))
right' _ = right_

right :: (Zipper h, Zipper h', Family f, Show (f h')) => f h' -> Loc h r (c :<: cs) -> ZipperR (Loc h' r (c :<: cs))
right v = either (Left . (++ " with type " ++ show v)) Right . right_

-- | Left

left_ :: (Zipper h, Zipper h') => Loc h r (c :<: cs) -> ZipperR (Loc h' r (c :<: cs))
left_ (Loc h (Push c cs)) = maybe (Left "Error going left") (\(h', c') -> Right (Loc h' (Push c' cs))) $ prev' c h

left' :: (Zipper h, Zipper h') => h' -> Loc h r (c :<: cs) -> ZipperR (Loc h' r (c :<: cs))
left' _ = left_

left :: (Zipper h, Zipper h', Family f, Show (f h')) => f h' -> Loc h r (c :<: cs) -> ZipperR (Loc h' r (c :<: cs))
left v = either (Left . (++ " with type " ++ show v)) Right . left_

-- | Zipper

class ( Representable  f
      , Typeable       f
      , Fillable  (Rep f)
      , Firstable (Rep f)
      , Nextable  (Rep f)
      , Lastable  (Rep f)
      , Prevable  (Rep f)) => Zipper f

instance Zipper Int
instance Zipper Char
instance Zipper Float
instance (Zipper a) => Zipper [a]

-- | Derivable

class Derivable f where
    data Derivative f :: * 
    
instance Derivable Int where
    data Derivative Int
    
instance Derivable U where
    data Derivative U
    
instance (Derivable f, Derivable g) => Derivable (f :+: g) where
    data Derivative (f :+: g) = CL (Derivative f) | CR (Derivative g)
    
instance (Derivable f, Derivable g) => Derivable (f :*: g) where
    data Derivative (f :*: g) = C1 (Derivative f) g | C2 f (Derivative g)
    
instance (Derivable a) => Derivable (Rec a) where
    data Derivative (Rec a) = Recursive
    
instance (Derivable a) => Derivable (Var a) where
    data Derivative (Var a) = Variable
    
instance (Derivable f) => Derivable (C c f) where
    data Derivative (C c f) = CC (Derivative f)

-- | Fill

class Fillable f where
    fill' :: (Typeable a) => Derivative f -> a -> Maybe f

instance Fillable U where
    fill' = impossible

instance Fillable Char where
    fill' = impossible

instance Fillable Int where
    fill' = impossible

instance Fillable Float where
    fill' = impossible
            
instance (Fillable f, Fillable g) => Fillable (f :+: g) where
--    fill :: Derivative (f :+: g) -> a -> (f :+: g)
    fill' (CL l) v = L <$> fill' l v
    fill' (CR r) v = R <$> fill' r v

instance (Fillable f, Fillable g) => Fillable (f :*: g) where
--    fill :: Derivative (f :+: g) -> a -> (f :+: g)
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
    first' :: (Zipper a) => f -> Maybe (a, Derivative f)

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

-- | Last

class Lastable f where
    last' :: (Zipper a) => f -> Maybe (a, Derivative f)

instance Lastable U where
    last' _ = Nothing

instance Lastable Char where
    last' _ = Nothing -- impossible?

instance Lastable Int where
    last' _ = Nothing -- impossible?
    
instance Lastable Float where
    last' _ = Nothing -- impossible?
            
instance (Lastable f, Lastable g) => Lastable (f :+: g) where
    last' (L x) = mapSnd CL <$> last' x
    last' (R y) = mapSnd CR <$> last' y

instance (Lastable f, Lastable g) => Lastable (f :*: g) where
    last' (l :*: r) = mapSnd (C2 l) <$> last' r
                  <|> mapSnd (flip C1 r) <$> last' l

instance (Typeable f) => Lastable (Rec f) where
    last' (Rec v) = (\x -> (x, Recursive)) <$> cast v

instance (Typeable f) => Lastable (Var f) where
    last' (Var v) = (\x -> (x, Variable)) <$> cast v

instance (Lastable f) => Lastable (C c f) where
    last' (C v) = mapSnd CC <$> last' v
    
-- | Next

class Nextable f where
    next' :: (Typeable a, Zipper b) => Derivative f -> a -> Maybe (b, Derivative f)
    
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

-- | Prev

class Prevable f where
    prev' :: (Typeable a, Zipper b) => Derivative f -> a -> Maybe (b, Derivative f)
    
instance Prevable U where
    prev' _ _ = Nothing

instance Prevable Char where
    prev' _ _ = Nothing

instance Prevable Int where
    prev' _ _ = Nothing

instance Prevable Float where
    prev' _ _ = Nothing

instance (Prevable f, Prevable g) => Prevable (f :+: g) where
    prev' (CL c) x = mapSnd CL <$> prev' c x
    prev' (CR c) y = mapSnd CR <$> prev' c y

instance (Lastable f, Fillable g, Prevable f, Prevable g) => Prevable (f :*: g) where
    prev' (C1 c y) x = mapSnd (flip C1 y) <$> prev' c x                    
    prev' (C2 x c) y = mapSnd (C2 x) <$> prev' c y 
                   <|> (\x' (y',c') -> (y', C1 c' x')) <$> fill' c y <*> last' x

instance Prevable (Rec f) where
    prev' (Recursive) _ = Nothing

instance Prevable (Var f) where
    prev' (Variable) _ = Nothing

instance (Prevable f) => Prevable (C c f) where
    prev' (CC v) x = mapSnd CC <$> prev' v x

-- | Type-level functions
