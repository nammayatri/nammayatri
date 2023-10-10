{-# LANGUAGE TemplateHaskell #-}

module Utils.TH
  ( genDBModelObjects,
  )
where

import Kernel.Beam.Lib.UtilsTH (IsDBTable, TableK)
import Kernel.Prelude as P
import Language.Haskell.TH as TH

genDBModelObjects :: Name -> Name -> TH.Q [TH.Dec]
genDBModelObjects appName dbModelName = do
  dbTypeConstructors <- genDBTypeConstructors dbModelName
  dbObjectType <- genDBObjectType dbTypeConstructors
  buildDBObjectFunc <- genBuildDBObject appName dbModelName dbTypeConstructors
  withDBObjectContentFunc <- genWithDBObjectContent appName dbTypeConstructors
  withFilteredDBObjectContentFunc <- genWithFilteredDBObjectContent appName dbModelName dbTypeConstructors
  pure $ (dbObjectType : buildDBObjectFunc) <> withDBObjectContentFunc <> withFilteredDBObjectContentFunc

genDBObjectType :: [DBTypeConstructor] -> Q Dec
genDBObjectType dbTypeConstructors = do
  let fName = mkName "f"
  let fType = VarT fName
  let typeName = mkName "DBObject"
  let defaultBang = Bang NoSourceUnpackedness NoSourceStrictness
  constructors <-
    forM dbTypeConstructors $ \dbTypeConstructor -> do
      pure $
        NormalC dbTypeConstructor.dbObjectConstructor
          [(defaultBang, fType `AppT` dbTypeConstructor.dbTableType)]
  let deriveClauses =
        [ DerivClause (Just StockStrategy) [ConT ''Generic]
        ]
  let fKind = InfixT (ConT ''TableK) (mkName "->") (ConT ''P.Type)
  pure $ DataD [] typeName [KindedTV fName fKind] Nothing constructors deriveClauses

genBuildDBObject :: Name -> Name -> [DBTypeConstructor] -> Q [TH.Dec]
genBuildDBObject appName dbModelName dbTypeConstructors = do
  let appNameT = pure $ TH.ConT appName
      dbModelT = pure $ TH.ConT dbModelName
      dbObjectT = pure . TH.ConT $ mkName "DBObject"
      dbModelN = TH.mkName "dbModel"
      dbModelP = pure $ TH.VarP dbModelN
      tableActionN = TH.mkName "tableAction"
      tableActionP = pure $ TH.VarP tableActionN
      tableActionE = pure $ TH.VarE tableActionN
  let tableCases = do
        matches <- forM dbTypeConstructors $ \dbTypeConstructor -> do
          tableCase <-
            [e|
              $(pure $ ConE dbTypeConstructor.dbObjectConstructor) <$> $tableActionE @($(pure dbTypeConstructor.dbTableType))
              |]
          pure $ TH.Match (TH.ConP dbTypeConstructor.dbModelConstructor []) (TH.NormalB tableCase) []
        pure $ TH.CaseE (TH.VarE dbModelN) matches

  [d|
    buildDBObject ::
      forall (f :: TableK -> P.Type) (m :: P.Type -> P.Type).
      Functor m =>
      $dbModelT ->
      (forall t. IsDBTable $appNameT t => m (f t)) ->
      m ($dbObjectT f)
    buildDBObject $dbModelP $tableActionP = $tableCases
    |]

genWithDBObjectContent :: Name -> [DBTypeConstructor] -> TH.Q [TH.Dec]
genWithDBObjectContent appName dbTypeConstructors = do
  let appNameT = pure $ TH.ConT appName
      dbObjectT = pure . TH.ConT $ TH.mkName "DBObject"
      dbObjectN = TH.mkName "dbObject"
      dbObjectP = pure $ TH.VarP dbObjectN
      actionN = TH.mkName "action"
      actionP = pure $ TH.VarP actionN
      actionE = pure $ TH.VarE actionN
      objN = TH.mkName "obj"
      objE = pure . TH.VarE $ objN
  let tableCases = do
        matches <- forM dbTypeConstructors $ \dbTypeConstructor -> do
          tableCase <-
            [e|
              $actionE @($(pure dbTypeConstructor.dbTableType)) $objE
              |]
          pure $ TH.Match (TH.ConP dbTypeConstructor.dbObjectConstructor [VarP objN]) (TH.NormalB tableCase) []
        pure $ TH.CaseE (TH.VarE dbObjectN) matches

  [d|
    withDBObjectContent ::
      forall (f :: TableK -> P.Type) (res :: P.Type).
      $dbObjectT f ->
      (forall t. IsDBTable $appNameT t => f t -> res) ->
      res
    withDBObjectContent $dbObjectP $actionP = $tableCases
    |]

genWithFilteredDBObjectContent :: Name -> Name -> [DBTypeConstructor] -> TH.Q [TH.Dec]
genWithFilteredDBObjectContent appName dbModelName dbTypeConstructors = do
  let appNameT = pure $ TH.ConT appName
      dbObjectT = pure . TH.ConT $ TH.mkName "DBObject"
      dbModelT = pure $ TH.ConT dbModelName
      dbModelN = TH.mkName "dbModel"
      dbModelP = pure $ TH.VarP dbModelN
      dbObjectsWithPayloadN = TH.mkName "dbObjectsWithPayload"
      dbObjectsWithPayloadP = pure $ TH.VarP dbObjectsWithPayloadN
      dbObjectsWithPayloadE = pure $ TH.VarE dbObjectsWithPayloadN
      actionN = TH.mkName "action"
      actionP = pure $ TH.VarP actionN
      actionE = pure $ TH.VarE actionN
      objN = TH.mkName "obj"
      objE = pure . TH.VarE $ objN
      payloadN = TH.mkName "payload"
      payloadE = pure . TH.VarE $ payloadN
      payloadP = pure . TH.VarP $ payloadN
  let tableCases = do
        matches <- forM dbTypeConstructors $ \dbTypeConstructor -> do
          let dbObjectP = pure (ConP dbTypeConstructor.dbObjectConstructor [VarP objN])
          tableCase <-
            [e|
              $actionE @($(pure dbTypeConstructor.dbTableType)) [($objE, $payloadE) | ($dbObjectP, $payloadP) <- $dbObjectsWithPayloadE]
              |]
          pure $ TH.Match (TH.ConP dbTypeConstructor.dbModelConstructor []) (TH.NormalB tableCase) []
        pure $ TH.CaseE (TH.VarE dbModelN) matches

  [d|
    withFilteredDBObjectContent ::
      forall (f :: TableK -> P.Type) (res :: P.Type) (payload :: P.Type).
      $dbModelT ->
      [($dbObjectT f, payload)] ->
      (forall t. IsDBTable $appNameT t => [(f t, payload)] -> res) ->
      res
    withFilteredDBObjectContent $dbModelP $dbObjectsWithPayloadP $actionP = $tableCases
    |]

data DBTypeConstructor = DBTypeConstructor
  { dbModelConstructor :: Name, -- TableOne :: DBModel
    dbObjectConstructor :: Name, -- TableOneObject :: DBObject
    dbTableType :: TH.Type -- qualified type TableOne.TableOneT
  }

genDBTypeConstructors :: Name -> Q [DBTypeConstructor]
genDBTypeConstructors dbModelName = do
  tableTypeInfo <- reify dbModelName
  case tableTypeInfo of
    TyConI dec -> do
      case dec of
        DataD _ _ _ _ constructors _ -> do
          forM constructors $ \constructor -> case constructor of
            NormalC constructorName [] -> do
              let constructorNameStr = nameBase constructorName
              pure
                DBTypeConstructor
                  { dbModelConstructor = constructorName,
                    dbObjectConstructor = mkName $ constructorNameStr <> "Object",
                    dbTableType = ConT . mkName $ constructorNameStr <> "." <> constructorNameStr <> "T"
                  }
            _ -> fail $ show constructor <> " constructor should be empty"
        _ -> fail $ show dbModelName <> " should be data declaration"
    _ -> fail $ show dbModelName <> " should be type name1"
