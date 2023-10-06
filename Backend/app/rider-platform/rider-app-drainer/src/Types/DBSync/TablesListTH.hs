{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
-- TODO check this is safe
{-# LANGUAGE UndecidableSuperClasses #-}

module Types.DBSync.TablesListTH where

import qualified Data.Serialize as Serialize
import Database.Beam.Postgres (Postgres)
import EulerHS.KVConnector.Types (MeshMeta)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Language.Haskell.TH as TH
import Sequelize (Model)
import qualified "rider-app" Storage.Beam.AppInstalls as AppInstalls
import qualified "rider-app" Storage.Beam.BlackListOrg as BlackListOrg
import qualified "rider-app" Storage.Beam.Booking as Booking

-- FIXME used only three table for simple refactor
tablesList :: [TH.Name]
tablesList =
  [ ''AppInstalls.AppInstalls,
    ''BlackListOrg.BlackListOrg,
    ''Booking.Booking
  ]

-- WARNING Be careful, "incomplete-patterns" warning does not present for TH-generated functions:
-- https://gitlab.haskell.org/ghc/ghc/-/issues/14838
-- So, we should use the same tableList for data type and for each pattern matching
-- For case expressions warnings also do not present
-- tablesTList :: [TH.Type]
-- tablesTList =
--   TH.ConT
--     <$> [ ''AppInstalls.AppInstallsT,
--           ''BlackListOrg.BlackListOrgT,
--           ''Booking.BookingT
--         ]

-- used qualified imports
tablesTList :: [TH.Type]
tablesTList = mkTName <$> tablesList
  where
    mkTName :: TH.Name -> TH.Type
    mkTName table = TH.ConT $ TH.mkName $ TH.nameBase table <> "." <> TH.nameBase table <> "T"

-- beam table constraint
type TableK = (Type -> Type) -> Type

class
  ( Sequelize.Model Postgres table,
    Show (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    MeshMeta Postgres table,
    Serialize.Serialize (table Identity)
  ) =>
  IsDbTable (table :: (Type -> Type) -> Type)

genDBModelObjects :: TH.Q [TH.Dec]
genDBModelObjects = do
  dbCreateObjectType <- genDBModelType
  isDbTableInstances <- genIsDbTableInstances
  buildDBObjectFunc <- genBuildDBObject
  withDBObjectContentFunc <- genWithDBObjectContent
  pure $ (dbCreateObjectType : isDbTableInstances) <> buildDBObjectFunc <> withDBObjectContentFunc

-- | A set of instances common for all identifier newtypes.
-- deriveWithDBObjectContent :: [TH.Name] -> TH.Q [TH.Dec]
-- deriveWithDBObjectContent _names = do
--   let names = [(''AppInstalls.AppInstallsT, 'AppInstalls), (''BlackListOrg.BlackListOrgT, 'BlackListOrg)]
--   let tableCases :: Q Exp = pure $ CaseE (VarE $ TH.mkName "riderModel") $ names <&> \(nameT, name) -> do

--         Match (ConP name []) (NormalB [e| withDBObjectContent' riderModel (cast @(f t) @(f AppInstalls.AppInstallsT) table) action|]) []
--   -- make DBModel constructors:
--   [d|

--     data DBModel = A | B
--       deriving stock (Show, Read)
--       -- AppInstalls | BecknRequest | BlackListOrg

--     withDBObjectContent ::
--       forall (f :: TableK -> Type) (m :: Type -> Type) (res :: Type) t.
--       ( MonadThrow m,
--         Log m,
--         IsDbTable t,
--         Typeable f
--       ) =>
--       DBModel ->
--       f t ->
--       (forall t1. IsDbTable t1 => f t1 -> m res) ->
--       m res
--     withDBObjectContent _riderModel _table _action = do
--       $tableCases
--       case riderModel of
--       --   AppInstalls -> withDBObjectContent' riderModel (cast @(f t) @(f AppInstalls.AppInstallsT) table) action
--       --   BecknRequest -> withDBObjectContent' riderModel (cast @(f t) @(f BecknRequest.BecknRequestT) table) action
--       --   _ -> error "TODO"
--     |]

-- withDBObjectContent ::
--   forall (f :: TableK -> Type) (m :: Type -> Type) (res :: Type) t.
--   ( MonadThrow m,
--     Log m,
--     IsDbTable t,
--     Typeable f
--   ) =>
--   DBModel ->
--   f t ->
--   (forall t1. IsDbTable t1 => f t1 -> m res) ->
--   m res
-- withDBObjectContent riderModel table action = [e| $tableCases |]
--   where
--     names = [(''AppInstalls.AppInstallsT, 'AppInstalls), (''BlackListOrg.BlackListOrgT, 'BlackListOrg)]
--     tableCases :: Q Exp = do
--       matches <- forM names $ \(nameT, name) -> do
--         tableCase <-
--           [e|
--               withDBObjectContent' riderModel (cast @(f t) @(f $(conT nameT)) table) action
--           |]
--         pure $ Match (ConP name []) (NormalB tableCase) []
--       pure $ CaseE (VarE $ TH.mkName "riderModel") matches

genWithDBObjectContent :: TH.Q [TH.Dec]
genWithDBObjectContent = do
  let riderModelP = pure . TH.VarP . TH.mkName $ "riderModel"
      tableP = pure . TH.VarP . TH.mkName $ "table"
      actionP = pure . TH.VarP . TH.mkName $ "action"
      dbModelT = pure . TH.ConT . TH.mkName $ "DBModel"
  [d|
    withDBObjectContent ::
      forall (f :: TableK -> Type) (m :: Type -> Type) (res :: Type) t.
      ( MonadThrow m,
        Log m,
        IsDbTable t,
        Typeable f
      ) =>
      $dbModelT ->
      f t ->
      (forall t1. IsDbTable t1 => f t1 -> m res) ->
      m res
    withDBObjectContent $riderModelP $tableP $actionP = $tableCases
    |]
  where
    names = zip tablesList tablesTList
    tableCases :: TH.Q TH.Exp = do
      matches <- forM names $ \(name, nameT) -> do
        let fT = pure . TH.VarT . TH.mkName $ "f"
        let tT = pure . TH.VarT . TH.mkName $ "t"
        tableCase <-
          [e|
            withDBObjectContent' riderModel (cast @($fT $tT) @($fT $(pure nameT)) table) action
            |]
        pure $ TH.Match (TH.ConP (TH.mkName . TH.nameBase $ name) []) (TH.NormalB tableCase) []
      pure $ TH.CaseE (TH.VarE $ TH.mkName "riderModel") matches

genBuildDBObject :: TH.Q [TH.Dec]
genBuildDBObject = do
  let constructorP = pure . TH.VarP . TH.mkName $ "constructor"
      riderModelP = pure . TH.VarP . TH.mkName $ "riderModel"
      tableActionP = pure . TH.VarP . TH.mkName $ "tableAction"
      dbModelT = pure . TH.ConT . TH.mkName $ "DBModel"
  [d|
    buildDBObject ::
      forall (f :: TableK -> Type) (m :: Type -> Type) (b :: Type).
      Monad m =>
      (forall t. IsDbTable t => $dbModelT -> f t -> b) ->
      $dbModelT ->
      (forall t. IsDbTable t => m (f t)) ->
      m b
    buildDBObject $constructorP $riderModelP $tableActionP = $tableCases
    |]
  where
    names = zip tablesList tablesTList
    tableCases :: TH.Q TH.Exp = do
      matches <- forM names $ \(name, nameT) -> do
        tableCase <-
          [e|
            constructor riderModel <$> tableAction @($(pure nameT))
            |]
        pure $ TH.Match (TH.ConP (TH.mkName . TH.nameBase $ name) []) (TH.NormalB tableCase) []
      pure $ TH.CaseE (TH.VarE $ TH.mkName "riderModel") matches

-- deriveWithDBObjectContent :: [TH.Name] -> TH.Q [TH.Dec]
-- deriveWithDBObjectContent _names = do
--   let names = [(''AppInstalls.AppInstallsT, 'AppInstalls), (''BlackListOrg.BlackListOrgT, 'BlackListOrg)]
--   let tableCases :: Q Exp = pure $ CaseE (VarE $ TH.mkName "riderModel") $ names <&> \(nameT, name) -> do
--         Match (ConP name []) (NormalB [e| withDBObjectContent' riderModel (cast @(f t) @(f AppInstalls.AppInstallsT) table) action|]) []
--   -- make DBModel constructors:
--   [d|

--       --   AppInstalls -> withDBObjectContent' riderModel (cast @(f t) @(f AppInstalls.AppInstallsT) table) action
--       --   BecknRequest -> withDBObjectContent' riderModel (cast @(f t) @(f BecknRequest.BecknRequestT) table) action
--       --   _ -> error "TODO"
--     |]

dbModelTypeName :: TH.Name
dbModelTypeName = TH.mkName "DBModel"

-- mkDBModelConstructorName :: TH.Name -> TH.Q TH.Name
-- mkDBModelConstructorName table = TH.mkName <$> getModelName table

-- TODO remove monadic actions
genDBModelType :: TH.Q TH.Dec
genDBModelType = do
  constructors <- forM tablesList $ \table -> do
    pure $ TH.NormalC (TH.mkName $ TH.nameBase table) []
  -- pure $ TH.NormalC (TH.mkName $ TH.nameBase table <> "M") []
  let deriveClauses =
        [ TH.DerivClause (Just TH.StockStrategy) [TH.ConT ''Show, TH.ConT ''Read, TH.ConT ''Enum, TH.ConT ''Bounded, TH.ConT ''Eq, TH.ConT ''Generic],
          TH.DerivClause (Just TH.AnyclassStrategy) [TH.ConT ''FromJSON]
        ]
  pure $ TH.DataD [] dbModelTypeName [] Nothing constructors deriveClauses

genIsDbTableInstances :: TH.Q [TH.Dec]
genIsDbTableInstances = forM tablesTList $ \tableT -> do
  let className = TH.mkName "IsDbTable"
  pure $ TH.InstanceD Nothing [] (TH.AppT (TH.ConT className) tableT) []

-- -- COMMON --

-- getModelName :: Name -> Q String
-- getModelName name = do
--   let name' = nameBase name
--   case last name' of
--     'T' -> pure $ take (length name' - 1) name'
--     _ -> fail $ "Table type should have suffix T: " <> name'

-- defaultBang :: Bang
-- defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

-- how to avoid foralls??
