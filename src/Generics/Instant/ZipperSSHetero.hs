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

{-# OPTIONS_GHC -Wall             #-}

module Generics.Instant.ZipperSSHetero where

import Data.Maybe
import Data.Typeable
import Debug.Trace

import Control.Applicative
import Control.Monad

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

-- | Initialization

createZipper :: (Zipper f) => f -> Loc f HNil
createZipper f = enter f

-- | Basic constructs

data HNil = HNil
data HCons e l = HCons e l

--data WCtx where
--    WCtx  :: Zipper f => Ctx (Rep f) -> WCtx
    
data Loc f c = Loc { val :: f, ctxs :: c }

enter :: Zipper f => f -> Loc f HNil
enter f = Loc f HNil

{-
leave :: Zipper f => Loc f -> f
leave (Loc f []) = f
leave loc = leave (fromMaybe (error "Error leaving") (up loc))
-}

up :: (Zipper f, Zipper f') => Loc f (HCons (Ctx (Rep f')) c) -> Maybe (Loc f' c)
up (Loc f (HCons c cs))   = (\x -> Loc (to x) cs) <$> fill' c f

down :: (Zipper f, Zipper f') => Loc f c -> Maybe (Loc f' (HCons (Ctx (Rep f)) c))
down (Loc f cs) = (\(f', c) -> Loc f' (HCons c cs)) <$> first' (from f)


--        where wrap :: (f', Ctx (Rep f)) -> Loc f'
--              wrap (f', c) = Loc f' (Push (fromJust . gcast $ c) cs)

-- | ZipperA

class (Representable f, Typeable f, Typeable (Rep f), Fillable (Rep f), Firstable (Rep f)) => Zipper f

-- | Zipper

class Zippable f where
    data Ctx f :: * 
    
instance Zippable Int where
    data Ctx Int
    
instance Zippable U where
    data Ctx U
    --fill ctx f = impossible
    --first _ U  = Nothing
    --last  _ U  = Nothing
    --next  _ _ _ = impossible
    --prev  _ _ _ = impossible
    
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

instance Fillable Int where
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
{-    
fill :: (Rewritable f, Rewritable f') => Ctx f -> f -> f'
fill = 

class Fillable f where
    fill        :: Ctx f -> f -> f
-}


-- | First

--first = undefined

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> d) -> (a,b) -> (a,d)
mapSnd f (a,b) = (a, f b)


class Firstable f where
    first' :: (Zipper a) => f -> Maybe (a, Ctx f)

instance Firstable U where
    first' _ = Nothing

instance Firstable Int where
    first' _ = Nothing
            
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

{-
class Nextable f where
    next' :: Ctx f -> f -> Maybe (a, Ctx f)
    
instance Nextable U where
    next' _ _ = impossible

instance (Nextable f, Nextable g) => Nextable (f :+: g) where
    first' (L x) = mapSnd CL <$> first' x
    first' (R y) = mapSnd CR <$> first' y

instance (Nextable f, Nextable g) => Nextable (f :*: g) where
    first' (l :*: r) = mapSnd (flip C1 r) <$> first' l
                   <|> mapSnd (C2 l) <$> first' r

instance (Nextable f) => Nextable (Rec f) where
    first' (Rec v) = (\x -> (x, Recursive)) <$> cast v

instance (Nextable f) => Nextable (Var f) where
    first' (Var v) = (\x -> (x, Variable)) <$> cast v

instance (Nextable f) => Nextable (C c f) where
    first' (C v) = mapSnd CC <$> first' v    
-}
    
