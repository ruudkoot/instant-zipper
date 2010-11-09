{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
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

fixDept :: Maybe Dept
fixDept =  return (enter dept)
        >>= down' intoEmployee
        >>= down' intoString
        >>= return . setHole "Prof. dr. Swierstra"
        >>= right' intoFloat
        >>= return . setHole 9000.0
        >>= up
        >>= up
        >>= downR' (intoList intoEmployee)
        >>= down' (intoList intoEmployee)
        >>= down' (intoList intoEmployee)
        >>= down' (intoEmployee)
        >>= downR' (intoFloat)
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

