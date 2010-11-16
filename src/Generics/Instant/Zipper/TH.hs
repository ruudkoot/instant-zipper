{-# LANGUAGE GADTs              #-}

module Generics.Instant.Zipper.TH where

{-
import Language.Haskell.TH
import Data.Generics
 
deriveFam :: Name -> Q [Dec]
deriveFam n = do datName <- newName "dat"
                 datPar <- newName "datPar"
                 --(TyConI       <- reify n         
                 return $ [DataD [] datName [PlainTV datPar] [] []]


allTypes :: Name ->  Q [Name]
allTypes n = do i <- reify n
                return (typesIn n) 

typesIn :: (Data a) => a -> [Name]
typesIn = everything (++) ([] `mkQ` qType)
    where qType :: Type -> [Name]
          qType (VarT n) = [n]
          qType (ConT n) = [n]
          qType _        = []

test :: Name -> Q [Dec]
test = do           
$( runQ (allTypes ''Maybe) >>= print)
-}
