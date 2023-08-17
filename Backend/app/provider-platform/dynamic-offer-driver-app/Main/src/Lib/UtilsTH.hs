module Lib.UtilsTH where

import qualified Data.Aeson as A
import Data.Cereal.Instances ()
import Data.Cereal.TH
import qualified Data.HashMap.Strict as HM
import Data.List (init, nub, (!!))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Beam as B
import Database.Beam.MySQL (MySQL)
import Database.Beam.Postgres (Postgres)
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), PrimaryKey (..), SecondaryKey (..), TermWrap (..))
import EulerHS.Prelude hiding (Type, words)
import Language.Haskell.TH
import qualified Sequelize as S
import Text.Casing (camel)
import Prelude (head)

enableKV :: Name -> [Name] -> [[Name]] -> Q [Dec]
enableKV name pKeyN sKeysN = do
  [tModeMeshSig, tModeMeshDec] <- tableTModMeshD name
  [kvConnectorDec] <- kvConnectorInstancesD name pKeyN sKeysN
  [meshMetaDec] <- meshMetaInstancesD name
  --   cerealDec <- cerealInstancesD name
  pure $ [tModeMeshSig, tModeMeshDec, meshMetaDec, kvConnectorDec] -- ++ cerealDec

enableKVPG :: Name -> [Name] -> [[Name]] -> Q [Dec]
enableKVPG name pKeyN sKeysN = do
  [tModeMeshSig, tModeMeshDec] <- tableTModMeshD name
  [kvConnectorDec] <- kvConnectorInstancesD name pKeyN sKeysN
  [meshMetaDec] <- meshMetaInstancesDPG name
  --   cerealDec <- cerealInstancesD name
  pure $ [tModeMeshSig, tModeMeshDec, meshMetaDec, kvConnectorDec] -- ++ cerealDec

-- DB.OrderReferenceT (B.FieldModification (B.TableField DB.OrderReferenceT)) add signature
tableTModMeshD :: Name -> Q [Dec]
tableTModMeshD name = do
  let tableTModMeshN = mkName $ camel (nameBase name) <> "ModMesh"
      psToHs = mkName "psToHs"
      applyFieldModifier field = AppE (AppE (AppE (VarE 'HM.findWithDefault) (LitE $ StringL field)) (LitE $ StringL field)) (VarE psToHs)
  names <- extractRecFields . head . extractConstructors <$> reify name
  let recExps = (\name' -> (name', AppE (VarE 'B.fieldNamed) (applyFieldModifier $ nameBase name'))) <$> names
  return
    [ SigD tableTModMeshN (AppT (ConT name) (AppT (ConT ''B.FieldModification) (AppT (ConT ''B.TableField) (ConT name)))),
      FunD tableTModMeshN [Clause [] (NormalB (RecUpdE (VarE 'B.tableModification) recExps)) []]
    ]

------------------------------------------------------------------
-- class KVConnector table where
--   tableName :: Text
--   keyMap :: HM.HashMap Text Bool -- True implies it is primary key and False implies secondary
--   primaryKey :: table -> PrimaryKey
--   secondaryKeys:: table -> [SecondaryKey]

kvConnectorInstancesD :: Name -> [Name] -> [[Name]] -> Q [Dec]
kvConnectorInstancesD name pKeyN sKeysN = do
  let tableName' = mkName "tableName"
      keyMap' = mkName "keyMap"
      primaryKey' = mkName "primaryKey"
      secondaryKeys' = mkName "secondaryKeys"

  let pKey = sortAndGetKey pKeyN
      sKeys = filter (\k -> pKey /= sortAndGetKey k) sKeysN
      pKeyPair = TupE [Just $ LitE $ StringL pKey, Just $ ConE 'True]
      sKeyPairs = map (\k -> TupE [Just $ LitE $ StringL $ sortAndGetKey k, Just $ ConE 'False]) sKeys

  let tableNameD = FunD tableName' [Clause [] (NormalB (LitE (StringL $ init $ camel (nameBase name)))) []]
      keyMapD = FunD keyMap' [Clause [] (NormalB (AppE (VarE 'HM.fromList) (ListE (pKeyPair : sKeyPairs)))) []]
      primaryKeyD = FunD primaryKey' [Clause [] (NormalB getPrimaryKeyE) []]
      secondaryKeysD = FunD secondaryKeys' [Clause [] (NormalB getSecondaryKeysE) []]

  return [InstanceD Nothing [] (AppT (ConT ''KVConnector) (AppT (ConT name) (ConT $ mkName "Identity"))) [tableNameD, keyMapD, primaryKeyD, secondaryKeysD]]
  where
    getPrimaryKeyE =
      let obj = mkName "obj"
       in LamE [VarP obj] (AppE (ConE 'PKey) (ListE (map (\n -> TupE [Just $ keyNameTextE n, Just $ getRecFieldE n obj]) pKeyN)))

    getSecondaryKeysE =
      if null sKeysN || sKeysN == [[]]
        then LamE [WildP] (ListE [])
        else do
          let obj = mkName "obj"
          LamE
            [VarP obj]
            ( ListE $
                map
                  (\sKey -> AppE (ConE 'SKey) (ListE (map (\n -> TupE [Just $ keyNameTextE n, Just $ getRecFieldE n obj]) sKey)))
                  sKeysN
            )

    getRecFieldE f obj =
      let fieldName = (splitColon $ nameBase f)
          instanceModifier = mkName $ (T.unpack . T.dropEnd 1 . T.pack $ camel (nameBase name)) <> "ToPSModifiers"
       in (AppE (AppE (AppE (VarE 'utilTransform) (VarE instanceModifier)) (LitE . StringL $ fieldName)) (AppE (VarE $ mkName fieldName) (VarE obj)))

    keyNameTextE n = AppE (VarE 'T.pack) (LitE $ StringL (splitColon $ nameBase n))

sortAndGetKey :: [Name] -> String
sortAndGetKey names = do
  let sortedKeys = sort $ fmap (splitColon . nameBase) names
  intercalate "_" sortedKeys

--"$sel:merchantId:OrderReference" -> "merchantId"
splitColon :: String -> String
splitColon s = T.unpack $ (T.splitOn ":" (T.pack s)) !! 1

-------------------------------------------------------------------------------

cerealInstancesD :: Name -> Q [Dec]
cerealInstancesD name = do
  tableCerealD <- makeCerealIdentity name
  types <- filter noCerealInstance <$> extractRecTypes . head . extractConstructors <$> reify name
  columnsCerealD <- concat <$> sequence (map makeCereal (nub types))
  return (columnsCerealD ++ tableCerealD)
  where
    noCerealInstance n = nameBase n `notElem` ["Text", "LocalTime", "Int", "Double", "Bool", "Int64", "Int32", "Scientific", "Value", "UTCTime"]

-------------------------------------------------------------------------------
meshMetaInstancesD :: Name -> Q [Dec]
meshMetaInstancesD name = do
  names <- extractRecFields . head . extractConstructors <$> reify name
  let meshModelFieldModification' = mkName "meshModelFieldModification"
      valueMapper' = mkName "valueMapper"
      modelTModMesh' = mkName $ camel (nameBase name) <> "ModMesh"
      modelToPSModifiers' = mkName $ camel (nameBase name) <> "oPSModifiers"

  let meshModelFieldModificationD = FunD meshModelFieldModification' [Clause [] (NormalB (VarE modelTModMesh')) []]
      valueMapperD = FunD valueMapper' [Clause [] (NormalB (VarE modelToPSModifiers')) []]

  let parseFieldAndGetClauseD = getParseFieldAndGetClauseD name names
      parseSetClauseD = getParseSetClauseD name names

  return [InstanceD Nothing [] (AppT (AppT (ConT ''MeshMeta) (ConT ''MySQL)) (ConT name)) [meshModelFieldModificationD, valueMapperD, parseFieldAndGetClauseD, parseSetClauseD]]

-------------------------------------------------------------------------------
meshMetaInstancesDPG :: Name -> Q [Dec]
meshMetaInstancesDPG name = do
  names <- extractRecFields . head . extractConstructors <$> reify name
  let meshModelFieldModification' = mkName "meshModelFieldModification"
      valueMapper' = mkName "valueMapper"
      modelTModMesh' = mkName $ camel (nameBase name) <> "ModMesh"
      modelToPSModifiers' = mkName $ camel (nameBase name) <> "oPSModifiers"

  let meshModelFieldModificationD = FunD meshModelFieldModification' [Clause [] (NormalB (VarE modelTModMesh')) []]
      valueMapperD = FunD valueMapper' [Clause [] (NormalB (VarE modelToPSModifiers')) []]

  let parseFieldAndGetClauseD = getParseFieldAndGetClauseD name names
      parseSetClauseD = getParseSetClauseD name names

  return [InstanceD Nothing [] (AppT (AppT (ConT ''MeshMeta) (ConT ''Postgres)) (ConT name)) [meshModelFieldModificationD, valueMapperD, parseFieldAndGetClauseD, parseSetClauseD]]

--------------- parseFieldAndGetClause instance -------------------
getParseFieldAndGetClauseD :: Name -> [Name] -> Dec
getParseFieldAndGetClauseD name names = do
  let fnName = mkName "parseFieldAndGetClause"
      obj = mkName "obj"
      field = mkName "fieldName"
      psToHs = mkName "psToHs"
  let patternMatches = (\n -> Match (LitP $ StringL (nameBase n)) (NormalB (parseFieldAndGetClauseE name n)) []) <$> names
      failExp = AppE (VarE 'fail) (LitE $ StringL ("Where clause decoding failed for " <> nameBase name <> " - Unexpected column " <> nameBase field))
  let matchE = AppE (AppE (AppE (VarE 'HM.findWithDefault) (VarE field)) (VarE field)) (VarE psToHs)
      caseExp = CaseE matchE (patternMatches ++ [Match WildP (NormalB failExp) []])
  FunD fnName [Clause [VarP obj, VarP field] (NormalB caseExp) []]

parseFieldAndGetClauseE :: Name -> Name -> Exp
parseFieldAndGetClauseE name key = do
  let v = mkName "obj"
  let parseExp = AppE (AppE (VarE 'parseField) (LitE $ StringL $ nameBase name)) (AppE (AppE (modifyFieldToHS name) (LitE $ StringL $ nameBase key)) (VarE v))
  AppE (AppE (VarE '(<$>)) (AppE (ConE 'TermWrap) (VarE key))) parseExp

--------------- parseSetClause instance -------------------
getParseSetClauseD :: Name -> [Name] -> Dec
getParseSetClauseD name names = do
  let fnName = mkName "parseSetClause"
      obj = mkName "obj"
      field = mkName "fieldName"
      parseKeyAndValue = mkName "parseKeyAndValue"
      setClause = mkName "setClause"
      psToHs = mkName "psToHs"

  let patternMatches = (\n -> Match (LitP $ StringL $ nameBase n) (NormalB (parseSetClauseE name n)) []) <$> names
      failExp = AppE (VarE 'fail) (LitE $ StringL ("Set clause decoding failed for " <> nameBase name <> " - Unexpected column " <> nameBase field))

  let matchE = AppE (AppE (AppE (VarE 'HM.findWithDefault) (VarE field)) (VarE field)) (VarE psToHs)
      caseExp = CaseE matchE (patternMatches ++ [Match WildP (NormalB failExp) []])
  FunD
    fnName
    [ Clause
        [VarP setClause]
        (NormalB (AppE (AppE (VarE 'mapM) (VarE parseKeyAndValue)) (VarE setClause)))
        [FunD parseKeyAndValue [Clause [TupP [VarP field, VarP obj]] (NormalB caseExp) []]]
    ]

parseSetClauseE :: Name -> Name -> Exp
parseSetClauseE name key = do
  let v = mkName "obj"
  let parseExp = AppE (AppE (VarE 'parseField) (LitE $ StringL $ nameBase name)) (AppE (AppE (modifyFieldToHS name) (LitE $ StringL $ nameBase key)) (VarE v))
  AppE (AppE (VarE '(<$>)) (AppE (ConE 'S.Set) (VarE key))) parseExp

------------------- Utils ------------------------
parseField :: (FromJSON a, MonadFail f) => Text -> A.Value -> f a
parseField modelName fieldObj = case A.fromJSON fieldObj of
  A.Success res -> pure res
  _ -> fail $ T.unpack $ "Error while decoding - Unable to parse field for " <> modelName <> " model"

-- modifyFieldToHS k = M.findWithDefault P.id k txnDetailToHSModifiers
modifyFieldToHS :: Name -> Exp
modifyFieldToHS name = do
  let toHsModifiers = mkName $ camel (nameBase name) <> "oHSModifiers"
      key = mkName "keyToBeModified"
      val = mkName "val"
  LamE [VarP key, VarP val] (AppE (AppE (AppE (AppE (VarE 'M.findWithDefault) (VarE 'id)) (VarE key)) (VarE toHsModifiers)) (VarE val))

extractConstructors :: Info -> [Con]
extractConstructors (TyConI (DataD _ _ _ _ cons _)) = cons
extractConstructors (TyConI (NewtypeD _ _ _ _ cons _)) = [cons]
extractConstructors _ = []

extractRecFields :: Con -> [Name]
extractRecFields (RecC _ bangs) = handleVarBang <$> bangs where handleVarBang (a, _, _) = a
extractRecFields _ = []

extractRecTypes :: Con -> [Name]
extractRecTypes (RecC _ bangs) = foldl' handleVarBang [] bangs
  where
    handleVarBang b (_, _, AppT _ (ConT n)) = n : b
    handleVarBang b (_, _, AppT _ (AppT _ (ConT n))) = n : b
    handleVarBang b _ = b
extractRecTypes _ = []

utilTransform :: (ToJSON a) => Map Text (A.Value -> A.Value) -> Text -> a -> Text
utilTransform modifyMap field value = do
  let res = case (M.lookup field modifyMap) of
        Just fn -> fn . A.toJSON $ value
        Nothing -> A.toJSON value
  case res of
    A.String r -> r
    A.Number n -> T.pack $ show n
    A.Bool b -> T.pack $ show b
    A.Array l -> T.pack $ show l
    A.Object o -> T.pack $ show o
    A.Null -> T.pack ""
