{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.SchemaTH (genToSchema) where

import Language.Haskell.TH
import Data.OpenApi (ToSchema)
import Kernel.Prelude hiding (Type)
import qualified Kernel.Types.Id
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Time.LocalTime (TimeOfDay)
import qualified Data.List as DL

-- | Generates 'ToSchema' instances for the given type and its fields recursively.
genToSchema :: Name -> Q [Dec]
genToSchema name = DL.nub <$> genToSchemaInternal [] name

genToSchemaInternal :: [Name] -> Name -> Q [Dec]
genToSchemaInternal seen name
  | name `elem` seen = pure []
  | isPrimitive name = pure []
  | isTypeConstructor name = pure []
  | otherwise = do
      exists <- isInstance ''ToSchema [ConT name]
      if exists
        then pure []
        else do
          info <- reify name
          let newSeen = name : seen
          case info of
            TyConI (DataD _ _ _ _ cons _) -> do
              innerDecs <- concat <$> mapM (genToSchemaForCon newSeen) cons
              pure $ innerDecs ++ [StandaloneDerivD Nothing [] (AppT (ConT ''ToSchema) (ConT name))]
            TyConI (NewtypeD _ _ _ _ con _) -> do
              innerDecs <- genToSchemaForCon newSeen con
              pure $ innerDecs ++ [StandaloneDerivD Nothing [] (AppT (ConT ''ToSchema) (ConT name))]
            _ -> pure []

isPrimitive :: Name -> Bool
isPrimitive name =
  name == ''Text
    || name == ''String
    || name == ''Int
    || name == ''Integer
    || name == ''Double
    || name == ''Float
    || name == ''Bool
    || name == ''UTCTime
    || name == ''NominalDiffTime
    || name == ''TimeOfDay

isTypeConstructor :: Name -> Bool
isTypeConstructor name =
  name == ''HashMap.HashMap
    || name == ''Map.Map
    || name == ''Set.Set
    || name == ''Vector.Vector
    || name == ''[]
    || name == ''Maybe
    || name == ''Either
    || name == ''Proxy

genToSchemaForCon :: [Name] -> Con -> Q [Dec]
genToSchemaForCon seen con = case con of
  NormalC _ fields -> concat <$> mapM (genToSchemaForType seen . snd) fields
  RecC _ fields -> concat <$> mapM (genToSchemaForType seen . (\(_, _, t) -> t)) fields
  InfixC (_, t1) _ (_, t2) -> (++) <$> genToSchemaForType seen t1 <*> genToSchemaForType seen t2
  ForallC _ _ c -> genToSchemaForCon seen c
  GadtC _ fields _ -> concat <$> mapM (genToSchemaForType seen . snd) fields
  RecGadtC _ fields _ -> concat <$> mapM (genToSchemaForType seen . (\(_, _, t) -> t)) fields

genToSchemaForType :: [Name] -> Type -> Q [Dec]
genToSchemaForType seen t = case t of
  ConT name -> genToSchemaInternal seen name
  AppT ListT t' -> genToSchemaForType seen t'
  AppT (ConT m) t' | m == ''Maybe -> genToSchemaForType seen t'
  AppT (ConT proxy) t' | proxy == ''Proxy -> genToSchemaForType seen t'
  AppT (ConT idt) _ | idt == ''Kernel.Types.Id.Id -> pure []
  AppT (ConT sidt) _ | sidt == ''Kernel.Types.Id.ShortId -> pure []
  AppT t1 t2 -> (++) <$> (genToSchemaForType seen t1) <*> (genToSchemaForType seen t2)
  _ -> pure []
