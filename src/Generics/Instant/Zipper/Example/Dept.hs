{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Generics.Instant.Zipper.Example.Dept where

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

data Fam a where
    Dept     :: Fam Dept
    Employee :: Fam Employee
    Salary   :: Fam Salary
    Name     :: Fam Name

deriving instance Show (Fam a)
    
instance Family Fam

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

fixDept :: ZipperR Dept
fixDept =  return (enter dept)
        >>= down  Employee
        >>= down  Name
        >>= return . set "Prof. dr. Swierstra"
        >>= right Salary
        >>= return . set 9000.0
        >>= return . up
        >>= return . up
        >>= downR (List Employee)
        >>= down  (List Employee)
        >>= down  (List Employee)
        >>= down  Employee
        >>= downR Salary
        >>= return . set 100.0
        >>= return . leave

