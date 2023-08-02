{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Utils where

import Data.ByteString.Internal (ByteString)
import qualified Data.Serialize as Serialize
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.DriverOffer as DomainDO
-- import qualified Domain.Types.FarePolicy.FareProductType as DomainFPT
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.CachedSqlDBQuery
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (KVConnector, MeshConfig (..), MeshMeta)
import qualified EulerHS.Language as L
import EulerHS.Types (BeamRunner, BeamRuntime, DBConfig, SqlConn)
import Kernel.Beam.Types
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Mesh as Mesh
import Sequelize
import System.Random

fromFieldMoney ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Money
fromFieldMoney f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Money <$> fromField f mbValue

instance FromField Context.City where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Context.City where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Context.City

instance FromBackendRow Postgres Context.City

instance FromField Context.Country where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Context.Country where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Context.Country

instance FromBackendRow Postgres Context.Country

instance FromField DomainDO.DriverOfferStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DomainDO.DriverOfferStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainDO.DriverOfferStatus

instance FromBackendRow Postgres DomainDO.DriverOfferStatus

instance IsString DomainDO.DriverOfferStatus where
  fromString = show

deriving stock instance Ord DomainDO.DriverOfferStatus

-- instance FromField DomainFPT.FareProductType where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be DomainFPT.FareProductType where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFPT.FareProductType

-- instance FromBackendRow Postgres DomainFPT.FareProductType

-- instance IsString DomainFPT.FareProductType where
--   fromString = show

-- instance FromField DomainDO.DriverOfferStatus where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be DomainDO.DriverOfferStatus where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainDO.DriverOfferStatus

-- instance FromBackendRow Postgres DomainDO.DriverOfferStatus

-- instance IsString DomainDO.DriverOfferStatus where
-- fromString = show

-- deriving stock instance Ord DomainDO.DriverOfferStatus

-- fromFieldJSON ::
--   (Typeable a, FromJSON a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldJSON f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' -> case A.decode $ fromStrict value' of
--     Just res -> pure res
--     Nothing -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Language where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance FromField Language => FromBackendRow Postgres Language

-- instance FromField Language where
--   fromField = fromFieldEnum

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Language

instance FromField Payment.Currency where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Payment.Currency where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Payment.Currency

instance FromBackendRow Postgres Payment.Currency

instance IsString Payment.Currency where
  fromString = show

deriving stock instance Ord Payment.Currency

instance FromField Payment.TransactionStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Payment.TransactionStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Payment.TransactionStatus

instance FromBackendRow Postgres Payment.TransactionStatus

instance IsString Payment.TransactionStatus where
  fromString = show

deriving stock instance Ord Payment.TransactionStatus

instance FromField VehVar.VehicleVariant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be VehVar.VehicleVariant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be VehVar.VehicleVariant

instance FromBackendRow Postgres VehVar.VehicleVariant

instance IsString VehVar.VehicleVariant where
  fromString = show

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case readMaybe (unpackChars value') of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> show value')

-- fromFieldEnumDbHash ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion DbHash
-- fromFieldEnumDbHash f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' -> pure $ DbHash value'

-- getPoint :: (Double, Double) -> BQ.QGenExpr context Postgres s Point
-- getPoint (lat, lon) = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_SetSRID (ST_Point (" <> show lon <> " , " <> show lat <> "),4326)"))

-- containsPoint'' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
-- containsPoint'' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (" <> show lon <> " , " <> show lat <> ")")))

-- containsPoint' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
-- containsPoint' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (geom, ST_GeomFromText('POINT (" <> show lon <> " " <> show lat <> ")'))")))

-- buildRadiusWithin' :: Point -> (Double, Double) -> Int -> BQ.QGenExpr context Postgres s BQ.SqlBool
-- buildRadiusWithin' pnt (lat, lon) rad =
--   BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_DWithin(" <> show pnt <> " , " <> getPoint' <> " , " <> show rad <> ")"))
--   where
--     getPoint' = "(SRID=4326;POINT(" <> show lon <> " " <> show lat <> "))"

(<->.) :: Point -> Point -> BQ.QGenExpr context Postgres s Double
(<->.) p1 p2 = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ show p1 <> " <-> " <> show p2))

setFlagsInMeshConfig :: (L.MonadFlow m) => MeshConfig -> Text -> m MeshConfig
setFlagsInMeshConfig meshCfg modelName = do
  let isMeshEnabled = isKVEnabled modelName
      isKVHardKilled = isHardKillEnabled modelName
  pure $ meshCfg {meshEnabled = isMeshEnabled, kvHardKilled = isKVHardKilled}
  where
    isKVEnabled _ = False
    isHardKillEnabled _ = True

-- enableKVForWriteAlso :: [Text]
-- enableKVForWriteAlso = [] -- ["app_installs", "black_list_org", "booking", "booking_cancellation_reason", "callback_request", "call_status", "cancellation_reason", "driver_offer", "estimate", "estimate_breakup", "exophone", "geometry", "issue", "merchant", "merchant_config", "on_search_event", "person", "quote", "registration_token", "rental_slab", "ride", "saved_location", "search_request", "sos", "special_zone_quote", "trip_terms", "webengage", "booking_location", "fare_breakup", "place_name_cache", "merchant_message", "merchant_payment_method", "merchant_service_config", "merchant_service_usage_config", "payment_order", "payment_transaction", "person_default_emergency_number", "person_flow_status", "search_request_location"]

-- enableKVForRead :: [Text]
-- enableKVForRead = [] -- ["app_installs", "black_list_org", "booking", "booking_cancellation_reason", "callback_request", "call_status", "cancellation_reason", "driver_offer", "estimate", "estimate_breakup", "exophone", "geometry", "issue", "merchant", "merchant_config", "on_search_event", "person", "quote", "registration_token", "rental_slab", "ride", "saved_location", "search_request", "sos", "special_zone_quote", "trip_terms", "webengage", "booking_location", "fare_breakup", "place_name_cache", "merchant_message", "merchant_payment_method", "merchant_service_config", "merchant_service_usage_config", "payment_order", "payment_transaction", "person_default_emergency_number", "person_flow_status", "search_request_location"]

setMeshConfig :: (L.MonadFlow m, HasCallStack) => Text -> m MeshConfig
setMeshConfig modelName = do
  tables <- L.getOption KBT.Tables
  randomIntV <- L.runIO (randomRIO (1, 100) :: IO Int)
  case tables of
    Nothing -> L.throwException $ InternalError "Tables not found"
    Just tables' -> do
      let enableKVForWriteAlso = tables'.enableKVForWriteAlso
      let enableKVForRead = tables'.enableKVForRead
      let tableAllocation = fromIntegral tables'.tableAllocation
      if randomIntV <= tableAllocation
        then pure $ meshConfig {meshEnabled = modelName `elem` enableKVForWriteAlso, kvHardKilled = modelName `notElem` enableKVForRead}
        else pure $ meshConfig {meshEnabled = False, kvHardKilled = modelName `notElem` enableKVForRead}

setMeshConfig' :: (L.MonadFlow m, HasCallStack) => Text -> MeshConfig -> m MeshConfig
setMeshConfig' modelName meshConfig' = do
  tables <- L.getOption KBT.Tables
  randomIntV <- L.runIO (randomRIO (1, 100) :: IO Int)
  case tables of
    Nothing -> L.throwException $ InternalError "Tables not found"
    Just tables' -> do
      let enableKVForWriteAlso = tables'.enableKVForWriteAlso
      let enableKVForRead = tables'.enableKVForRead
      let tableAllocation = fromIntegral tables'.tableAllocation
      if randomIntV <= tableAllocation
        then pure $ meshConfig' {meshEnabled = modelName `elem` enableKVForWriteAlso, kvHardKilled = modelName `notElem` enableKVForRead}
        else pure $ meshConfig' {meshEnabled = False, kvHardKilled = modelName `notElem` enableKVForRead}

class
  FromTType' t a
    | t -> a
  where
  fromTType' :: (MonadThrow m, Log m, L.MonadFlow m) => t -> m (Maybe a)

class
  ToTType' t a
    | a -> t
  where
  toTType' :: a -> t

findOneWithKV ::
  forall table m a.
  ( HasCallStack,
    FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  -- DBConfig beM ->
  Where Postgres table ->
  m (Maybe a)
-- m (Maybe (table Identity))
findOneWithKV where' = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf' <- getMasterDBConfig
  result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right (Just res) -> fromTType' res
    Right Nothing -> pure Nothing
    Left err -> throwError $ InternalError $ show err

findAllWithKV ::
  forall table m a.
  ( HasCallStack,
    FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  -- DBConfig beM ->
  Where Postgres table ->
  m [a]
-- m (Maybe (table Identity))
findAllWithKV where' = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf' <- getMasterDBConfig
  result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsKV ::
  forall table m a.
  ( HasCallStack,
    FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  -- DBConfig beM ->
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
-- m (Maybe (table Identity))
findAllWithOptionsKV where' orderBy mbLimit mbOffset = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf <- getMasterDBConfig
  result <- KV.findAllWithOptionsKVConnector dbConf updatedMeshConfig where' orderBy mbLimit mbOffset
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

updateWithKV ::
  forall table m.
  ( HasCallStack,
    -- FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    SqlReturning Pg Postgres,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  -- DBConfig beM ->throwError
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
-- m (Maybe (table Identity))
updateWithKV setClause whereClause = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf <- getMasterDBConfig
  res <- KV.updateAllWithKVConnector dbConf updatedMeshConfig setClause whereClause
  case res of
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ show err

updateOneWithKV ::
  forall table m.
  ( HasCallStack,
    -- FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    SqlReturning Pg Postgres,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  -- DBConfig beM ->throwError
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
-- m (Maybe (table Identity))
updateOneWithKV setClause whereClause = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf <- getMasterDBConfig
  res <- KV.updateWoReturningWithKVConnector dbConf updatedMeshConfig setClause whereClause
  case res of
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ show err

createWithKV ::
  forall table m a.
  ( HasCallStack,
    ToTType' (table Identity) a,
    SqlReturning Pg Postgres,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  -- DBConfig Pg ->
  a ->
  m ()
createWithKV a = do
  let tType = toTType' a
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf' <- getMasterDBConfig
  result <- KV.createWoReturingKVConnector dbConf' updatedMeshConfig tType
  case result of
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ show err

deleteWithKV ::
  forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    SqlReturning beM be,
    B.HasQBuilder be,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Log m,
    Show (table Identity),
    MonadThrow m,
    SqlReturning Pg be,
    BeamRuntime be Pg
  ) =>
  Where be table ->
  m ()
deleteWithKV whereClause = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf <- getMasterDBConfig
  res <- KV.deleteAllReturningWithKVConnector dbConf updatedMeshConfig whereClause
  case res of
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ show err

getMasterDBConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
getMasterDBConfig = do
  dbConf <- L.getOption PsqlDbCfg
  case dbConf of
    Just dbCnf' -> pure dbCnf'
    Nothing -> L.throwException $ InternalError "DB Config not found"

getMasterBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
getMasterBeamConfig = do
  dbConf <- getMasterDBConfig
  conn <- L.getOrInitSqlConn dbConf
  case conn of
    Right conn' -> pure conn'
    Left _ -> L.throwException $ InternalError "DB Config not found"

----- replica db funcitons---------------

getReplicaDbConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
getReplicaDbConfig = do
  dbConf <- L.getOption PsqlDbCfgR1
  case dbConf of
    Just dbCnf' -> pure dbCnf'
    Nothing -> L.throwException $ InternalError "DB Config not found"

getReplicaBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
getReplicaBeamConfig = do
  dbConf <- getReplicaDbConfig
  conn <- L.getOrInitSqlConn dbConf
  case conn of
    Right conn' -> pure conn'
    Left _ -> L.throwException $ InternalError "DB Config not found"

findAllWithKvInReplica ::
  forall table m a.
  ( HasCallStack,
    FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  Where Postgres table ->
  m [a]
findAllWithKvInReplica where' = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf <- getReplicaDbConfig
  result <- KV.findAllWithKVConnector dbConf updatedMeshConfig where'
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findAllWithOptionsKvInReplica ::
  forall table m a.
  ( HasCallStack,
    FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  Where Postgres table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m [a]
findAllWithOptionsKvInReplica where' orderBy mbLimit mbOffset = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf <- getReplicaDbConfig
  result <- KV.findAllWithOptionsKVConnector dbConf updatedMeshConfig where' orderBy mbLimit mbOffset
  case result of
    Right res -> do
      res' <- mapM fromTType' res
      pure $ catMaybes res'
    Left err -> throwError $ InternalError $ show err

findOneWithKvInReplica ::
  forall table m a.
  ( HasCallStack,
    FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  Where Postgres table ->
  m (Maybe a)
findOneWithKvInReplica where' = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf' <- getReplicaDbConfig
  result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
  case result of
    Right (Just res) -> fromTType' res
    Right Nothing -> pure Nothing
    Left err -> throwError $ InternalError $ show err

updateWithKvInReplica ::
  forall table m.
  ( HasCallStack,
    -- FromTType' (table Identity) a,
    BeamRuntime Postgres Pg,
    SqlReturning Pg Postgres,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  [Set Postgres table] ->
  Where Postgres table ->
  m ()
updateWithKvInReplica setClause whereClause = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf <- getReplicaDbConfig
  res <- KV.updateAllWithKVConnector dbConf updatedMeshConfig setClause whereClause
  case res of
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ show err

createWithKvInReplica ::
  forall table m a.
  ( HasCallStack,
    ToTType' (table Identity) a,
    SqlReturning Pg Postgres,
    BeamRuntime Postgres Pg,
    B.HasQBuilder Postgres,
    BeamRunner Pg,
    Model Postgres table,
    MeshMeta Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity),
    Log m,
    MonadThrow m
  ) =>
  a ->
  m ()
createWithKvInReplica a = do
  let tType = toTType' a
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf' <- getReplicaDbConfig
  result <- KV.createWoReturingKVConnector dbConf' updatedMeshConfig tType
  case result of
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ show err

deleteWithKvInReplica ::
  forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    SqlReturning beM be,
    B.HasQBuilder be,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Log m,
    Show (table Identity),
    MonadThrow m,
    SqlReturning Pg be,
    BeamRuntime be Pg
  ) =>
  Where be table ->
  m ()
deleteWithKvInReplica whereClause = do
  updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
  dbConf <- getReplicaDbConfig
  res <- KV.deleteAllReturningWithKVConnector dbConf updatedMeshConfig whereClause
  case res of
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ show err
