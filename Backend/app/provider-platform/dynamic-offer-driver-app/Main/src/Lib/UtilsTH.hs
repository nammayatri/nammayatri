{-# LANGUAGE TemplateHaskellQuotes #-}

module Lib.UtilsTH where

import qualified Data.Aeson as A
import Data.Cereal.Instances ()
import Data.Cereal.TH
import qualified Data.HashMap.Internal as HMI
import qualified Data.HashMap.Strict as HM
import Data.List (init, nub, (!!))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Beam as B
import Database.Beam.MySQL (MySQL)
import Database.Beam.Postgres (Postgres)
import qualified Database.Beam.Schema.Tables as B
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), PrimaryKey (..), SecondaryKey (..), TermWrap (..))
import EulerHS.Prelude hiding (Type, words)
import Language.Haskell.TH
import Sequelize
import qualified Sequelize as S
import Text.Casing (camel)
import Prelude (head)
import qualified Prelude as P

emptyTextHashMap :: HMI.HashMap Text Text
emptyTextHashMap = HMI.empty

emptyValueHashMap :: M.Map Text (A.Value -> A.Value)
emptyValueHashMap = M.empty

enableKV :: Name -> [Name] -> [[Name]] -> Q [Dec]
enableKV name pKeyN sKeysN = do
  [tModeMeshSig, tModeMeshDec] <- tableTModMeshD name
  [kvConnectorDec] <- kvConnectorInstancesD name pKeyN sKeysN
  [meshMetaDec] <- meshMetaInstancesD name
  pure [tModeMeshSig, tModeMeshDec, meshMetaDec, kvConnectorDec] -- ++ cerealDec

enableKVPG :: Name -> [Name] -> [[Name]] -> Q [Dec]
enableKVPG name pKeyN sKeysN = do
  [tModeMeshSig, tModeMeshDec] <- tableTModMeshD name
  [kvConnectorDec] <- kvConnectorInstancesD name pKeyN sKeysN
  [meshMetaDec] <- meshMetaInstancesDPG name
  pure [tModeMeshSig, tModeMeshDec, meshMetaDec, kvConnectorDec] -- ++ cerealDec

tableTModMeshD :: Name -> Q [Dec]
tableTModMeshD name = do
  let tableTModMeshN = mkName $ camel (nameBase name) <> "ModMesh"
  names <- extractRecFields . head . extractConstructors <$> reify name
  let recExps = (\name' -> (name', AppE (VarE 'B.fieldNamed) (LitE $ StringL $ nameBase name'))) <$> names
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
                  (AppE (ConE 'SKey) . ListE . map (\n -> TupE [Just $ keyNameTextE n, Just $ getRecFieldE n obj]))
                  sKeysN
            )
    getRecFieldE f obj =
      let fieldName = (splitColon $ nameBase f)
       in AppE (AppE (AppE (VarE 'utilTransform) (VarE 'emptyValueHashMap)) (LitE . StringL $ fieldName)) (AppE (VarE $ mkName fieldName) (VarE obj))
    keyNameTextE n = AppE (VarE 'T.pack) (LitE $ StringL (splitColon $ nameBase n))

sortAndGetKey :: [Name] -> String
sortAndGetKey names = do
  let sortedKeys = sort $ fmap (splitColon . nameBase) names
  intercalate "_" sortedKeys

--"$sel:merchantId:OrderReference" -> "merchantId"
splitColon :: String -> String
splitColon s = T.unpack $ T.splitOn ":" (T.pack s) !! 1

-------------------------------------------------------------------------------
cerealInstancesD :: Name -> Q [Dec]
cerealInstancesD name = do
  tableCerealD <- makeCerealIdentity name
  types <- filter noCerealInstance . extractRecTypes . head . extractConstructors <$> reify name
  columnsCerealD <- concat <$> mapM makeCereal (nub types)
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
  let meshModelFieldModificationD = FunD meshModelFieldModification' [Clause [] (NormalB (VarE modelTModMesh')) []]
      valueMapperD = FunD valueMapper' [Clause [] (NormalB (VarE 'emptyValueHashMap)) []]
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
  let meshModelFieldModificationD = FunD meshModelFieldModification' [Clause [] (NormalB (VarE modelTModMesh')) []]
      valueMapperD = FunD valueMapper' [Clause [] (NormalB (VarE 'emptyValueHashMap)) []]
  let parseFieldAndGetClauseD = getParseFieldAndGetClauseD name names
      parseSetClauseD = getParseSetClauseD name names
  return [InstanceD Nothing [] (AppT (AppT (ConT ''MeshMeta) (ConT ''Postgres)) (ConT name)) [meshModelFieldModificationD, valueMapperD, parseFieldAndGetClauseD, parseSetClauseD]]

--------------- parseFieldAndGetClause instance -------------------
getParseFieldAndGetClauseD :: Name -> [Name] -> Dec
getParseFieldAndGetClauseD name names = do
  let fnName = mkName "parseFieldAndGetClause"
      obj = mkName "obj"
      field = mkName "fieldName"
  let patternMatches = (\n -> Match (LitP $ StringL (nameBase n)) (NormalB (parseFieldAndGetClauseE name n)) []) <$> names
      failExp = AppE (VarE 'fail) (LitE $ StringL ("Where clause decoding failed for " <> nameBase name <> " - Unexpected column " <> nameBase field))
  let matchE = AppE (AppE (AppE (VarE 'HM.findWithDefault) (VarE field)) (VarE field)) (VarE 'emptyTextHashMap)
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
  let patternMatches = (\n -> Match (LitP $ StringL $ nameBase n) (NormalB (parseSetClauseE name n)) []) <$> names
      failExp = AppE (VarE 'fail) (LitE $ StringL ("Set clause decoding failed for " <> nameBase name <> " - Unexpected column " <> nameBase field))
  let matchE = AppE (AppE (AppE (VarE 'HM.findWithDefault) (VarE field)) (VarE field)) (VarE 'emptyTextHashMap)
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
modifyFieldToHS _ = do
  let key = mkName "keyToBeModified"
      val = mkName "val"
  LamE [VarP key, VarP val] (AppE (AppE (AppE (AppE (VarE 'M.findWithDefault) (VarE 'P.id)) (VarE key)) (VarE 'emptyValueHashMap)) (VarE val))

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
  let res = case M.lookup field modifyMap of
        Just fn -> fn . A.toJSON $ value
        Nothing -> A.toJSON value
  case res of
    A.String r -> r
    A.Number n -> T.pack $ show n
    A.Bool b -> T.pack $ show b
    A.Array l -> T.pack $ show l
    A.Object o -> T.pack $ show o
    A.Null -> T.pack ""

mkEmod :: Name -> String -> String -> Q [Dec]
mkEmod name table schema = do
  let fnName = mkName $ (T.unpack . T.dropEnd 1 . T.pack $ camel (nameBase name)) <> "Table"
      tableTModN = mkName $ camel (nameBase name) <> "Mod"
      bodyExpr =
        InfixE
          (Just (AppE (VarE 'B.setEntitySchema) (AppE (ConE 'Just) (LitE $ StringL schema))))
          (VarE '(<>))
          ( Just
              ( InfixE
                  (Just (AppE (VarE 'B.setEntityName) (LitE $ StringL table)))
                  (VarE '(<>))
                  ( Just
                      ( AppE
                          (VarE 'B.modifyTableFields)
                          (VarE tableTModN)
                      )
                  )
              )
          )
  -- return [FunD fnName [Clause [] (NormalB bodyExpr) []]]
  return [FunD fnName [Clause [] (NormalB bodyExpr) []]]

mkModelMetaInstances :: Name -> String -> String -> Q [Dec]
mkModelMetaInstances name table schema = do
  let modelFM = mkName "modelFieldModification"
      modelTable = mkName "modelTableName"
      modelSchema = mkName "modelSchemaName"
      tableTModN = mkName $ camel (nameBase name) <> "Mod"
      modelFMInstance = FunD modelFM [Clause [] (NormalB (VarE tableTModN)) []]
      modelNameInstance = FunD modelTable [Clause [] (NormalB (LitE $ StringL table)) []]
      modelSchemaInstance = FunD modelSchema [Clause [] (NormalB (AppE (ConE 'Just) (LitE $ StringL schema))) []]
  return [InstanceD Nothing [] (AppT (ConT ''ModelMeta) (ConT name)) [modelFMInstance, modelNameInstance, modelSchemaInstance]]

mkSerialInstances :: Name -> Q [Dec]
mkSerialInstances name = do
  let -- tName     = mkName $ (T.unpack . T.dropEnd 1 . T.pack $ camel (nameBase name))
      putFn = mkName "put"
      getFn = mkName "get"
      putInstance = FunD putFn [Clause [] (NormalB (AppE (VarE 'error) (LitE $ StringL ""))) []]
      getInstance = FunD getFn [Clause [] (NormalB (AppE (VarE 'error) (LitE $ StringL ""))) []]
  return [InstanceD Nothing [] (AppT (ConT ''Serialize) (AppT (ConT name) (ConT $ mkName "Identity"))) [putInstance, getInstance]]

mkFromJSONInstance :: Name -> Q [Dec]
mkFromJSONInstance name = do
  let fromJSONFn = mkName "parseJSON"
      fromJSONInstance = FunD fromJSONFn [Clause [] (NormalB (AppE (VarE 'A.genericParseJSON) (VarE 'A.defaultOptions))) []]
  return [InstanceD Nothing [] (AppT (ConT ''FromJSON) (AppT (ConT name) (ConT $ mkName "Identity"))) [fromJSONInstance]]

mkToJSONInstance :: Name -> Q [Dec]
mkToJSONInstance name = do
  let toJSONFn = mkName "toJSON"
      toJSONInstance = FunD toJSONFn [Clause [] (NormalB (AppE (VarE 'A.genericToJSON) (VarE 'A.defaultOptions))) []]
  return [InstanceD Nothing [] (AppT (ConT ''ToJSON) (AppT (ConT name) (ConT $ mkName "Identity"))) [toJSONInstance]]

mkShowInstance :: Name -> Q [Dec]
mkShowInstance name = do
  return [StandaloneDerivD (Just StockStrategy) [] (AppT (ConT ''Show) (AppT (ConT name) (ConT $ mkName "Identity")))]

mkTableInstances :: Name -> String -> String -> Q [Dec]
mkTableInstances name table schema = do
  [modelMetaInstances] <- mkModelMetaInstances name table schema
  [eModInstances] <- mkEmod name table schema
  [serialInstances] <- mkSerialInstances name
  [fromJSONInstances] <- mkFromJSONInstance name
  [toJSONInstances] <- mkToJSONInstance name
  [showInstances] <- mkShowInstance name
  pure [modelMetaInstances, eModInstances, serialInstances, fromJSONInstances, toJSONInstances, showInstances]
