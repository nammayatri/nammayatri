{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.SchemaTH (genToSchema) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Time.LocalTime (TimeOfDay)
import qualified Data.Vector as Vector
import Kernel.Prelude hiding (Type)
import qualified Kernel.Types.Id
import Language.Haskell.TH

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
          -- Only emit a top-level deriving for nullary (fully-saturated) type constructors;
          -- HKD/parameterised types are reached via their applied form (see genToSchemaForAppliedType).
          TyConI (DataD _ _ tvs _ cons _) -> do
            innerDecs <- concat <$> mapM (genToSchemaForCon newSeen) cons
            pure $ innerDecs ++ [StandaloneDerivD Nothing [] (AppT (ConT ''ToSchema) (ConT name)) | null tvs]
          TyConI (NewtypeD _ _ tvs _ con _) -> do
            innerDecs <- genToSchemaForCon newSeen con
            pure $ innerDecs ++ [StandaloneDerivD Nothing [] (AppT (ConT ''ToSchema) (ConT name)) | null tvs]
          -- Type synonyms (e.g. `type Foo = FooD 'Safe`): resolve to the applied data type.
          TyConI (TySynD _ _ rhs) -> genToSchemaForAppliedType newSeen rhs
          _ -> pure []

-- | Generate 'ToSchema' for a fully-applied data type (e.g. @FooD 'Safe@): recurse into the
-- (type-argument-substituted) fields and emit a standalone deriving for the applied head.
-- Returns nothing for partial applications, type families, and already-derived heads.
genToSchemaForAppliedType :: [Name] -> Type -> Q [Dec]
genToSchemaForAppliedType seen ty =
  case unapplyType ty of
    (ConT realName, args)
      | realName `elem` seen -> pure []
      | otherwise -> do
        info <- reify realName
        let newSeen = realName : seen
            deriveWith tvs cons
              | length tvs /= length args = pure [] -- not fully saturated; skip (avoids ill-kinded heads)
              | otherwise = do
                exists <- isInstance ''ToSchema [ty]
                if exists
                  then pure []
                  else do
                    let cons' = map (substCon (mkSubst tvs args)) cons
                    innerDecs <- concat <$> mapM (genToSchemaForCon newSeen) cons'
                    pure $ innerDecs ++ [StandaloneDerivD Nothing [] (AppT (ConT ''ToSchema) ty)]
        case info of
          TyConI (DataD _ _ tvs _ cons _) -> deriveWith tvs cons
          TyConI (NewtypeD _ _ tvs _ con _) -> deriveWith tvs [con]
          _ -> pure []
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
  AppT t1 t2 -> (++) <$> genToSchemaForType seen t1 <*> genToSchemaForType seen t2
  _ -> pure []

-- | Split a (possibly applied) type into its head constructor and arguments.
unapplyType :: Type -> (Type, [Type])
unapplyType = go []
  where
    go acc (AppT f x) = go (x : acc) f
    go acc (SigT ty _) = go acc ty
    go acc (ParensT ty) = go acc ty
    go acc ty = (ty, acc)

tyVarName :: TyVarBndr flag -> Name
tyVarName (PlainTV n _) = n
tyVarName (KindedTV n _ _) = n

mkSubst :: [TyVarBndr flag] -> [Type] -> [(Name, Type)]
mkSubst tvs args = zip (map tyVarName tvs) args

substType :: [(Name, Type)] -> Type -> Type
substType sub = go
  where
    go (VarT n) = fromMaybe (VarT n) (lookup n sub)
    go (AppT a b) = AppT (go a) (go b)
    go (AppKindT ty k) = AppKindT (go ty) k
    go (SigT ty k) = SigT (go ty) k
    go (InfixT a n b) = InfixT (go a) n (go b)
    go (ParensT ty) = ParensT (go ty)
    go ty = ty

substField :: [(Name, Type)] -> VarBangType -> VarBangType
substField sub (n, b, t) = (n, b, substType sub t)

substCon :: [(Name, Type)] -> Con -> Con
substCon sub con = case con of
  NormalC n bts -> NormalC n (map (\(b, t) -> (b, substType sub t)) bts)
  RecC n vbts -> RecC n (map (substField sub) vbts)
  InfixC (b1, t1) n (b2, t2) -> InfixC (b1, substType sub t1) n (b2, substType sub t2)
  ForallC vs cx c -> ForallC vs cx (substCon sub c)
  GadtC ns bts t -> GadtC ns (map (\(b, t') -> (b, substType sub t')) bts) (substType sub t)
  RecGadtC ns vbts t -> RecGadtC ns (map (substField sub) vbts) (substType sub t)
