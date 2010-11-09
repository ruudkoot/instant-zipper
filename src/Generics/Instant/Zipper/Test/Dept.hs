{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Generics.Instant.Zipper.Test.Dept where

import Generics.Instant
import Generics.Instant.TH
import Generics.Instant.Zipper

-- | Datatype

type Salary    = Float
type Manager   = Employee
type Name      = String

data Dept = D Manager [Employee]
    deriving (Eq, Show, Typeable)

data Employee = E Name Salary
    deriving (Eq, Show, Typeable)

-- | Representation

$(deriveAll ''Dept)
$(deriveAll ''Employee)

-- | Family

data Family a where
    Dept     ::             Family Dept
    Employee ::             Family Employee
    Salary   ::             Family Salary
    Name     ::             Family Name
    List     :: Family a -> Family [a]
    deriving Show
    
instance Family Family

-- | Zipper

instance Zipper Dept
instance Zipper Employee
    
-- | Example

dept :: Dept
dept = D doaitse [johan, sean, pedro]
    where doaitse, johan, sean, pedro :: Employee
          doaitse  = E "Doaitse"  8000
          johan    = E "Johan"    8000
          sean     = E "Sean"     2600
          pedro    = E "Pedro"    2400

fixDept :: Maybe Dept
fixDept =  return (enter dept)
        >>= down  Employee
        >>= down  Name
        >>= return . setHole "Prof. dr. Swierstra"
        >>= right Salary
        >>= return . setHole 9000.0
        >>= up
        >>= up
        >>= downR (List Employee)
        >>= down  (List Employee)
        >>= down  (List Employee)
        >>= down  Employee
        >>= downR Salary
        >>= return . setHole 100.0
        >>= return . leave

