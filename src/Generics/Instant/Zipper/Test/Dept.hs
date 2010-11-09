{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Generics.Instant.Zipper.Test.Dept where

import Data.Maybe
import Data.Typeable

import Generics.Instant
import Generics.Instant.TH
import Generics.Instant.Zipper

-- | Datatype

data Dept = D Manager [Employee]
    deriving (Eq, Show, Typeable)

data Employee = E Name Salary
    deriving (Eq, Show, Typeable)

type Salary    = Float
type Manager   = Employee
type Name      = String


-- | Representation

$(deriveAll ''Dept)
$(deriveAll ''Employee)

-- | Zipper

instance Zipper Dept
instance Zipper Employee
    
-- | dept

dept :: Dept
dept = D doaitse [johan, sean, pedro]
    where doaitse, johan, sean, pedro :: Employee
          doaitse  = E "Doaitse"  8000
          johan    = E "Johan"    8000
          sean     = E "Sean"     2600
          pedro    = E "Pedro"    2400

data Fam a where
    Employee     ::          Fam Employee
    Float        ::          Fam Float
    String       ::          Fam String
    List         :: Fam a -> Fam [a]
    deriving Show
    
instance Family Fam

fixDept :: Maybe Dept
fixDept =  return (enter dept)
        >>= down' Employee
        >>= down' String
        >>= return . setHole "Prof. dr. Swierstra"
        >>= right' Float
        >>= return . setHole 9000.0
        >>= up
        >>= up
        >>= downR' (List Employee)
        >>= down' (List Employee)
        >>= down' (List Employee)
        >>= down' Employee
        >>= downR' Float
        >>= return . setHole 100.0
        >>= return . leave


intoDept :: Dept
intoDept = undefined

intoEmployee :: Employee
intoEmployee = undefined

intoList :: a -> [a]
intoList _ = undefined

intoFloat :: Float
intoFloat = undefined

intoString :: String
intoString = undefined

