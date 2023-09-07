{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Utils where

import Data.ByteString.Internal (ByteString, unpackChars)
import Data.Fixed (Centi)
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend hiding (tableName)
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.FarePolicy as DomainFP
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common (encodeToText)

fromFieldCenti ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Centi
fromFieldCenti f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case readMaybe (unpackChars value') of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

fromFieldCentesimal ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Centesimal
fromFieldCentesimal f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Centesimal <$> fromField f mbValue

-- fromFieldMinutes ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion Minutes
-- fromFieldMinutes f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just _ -> Minutes <$> fromField f mbValue

fromFieldMeters ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Meters
fromFieldMeters f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Meters <$> fromField f mbValue

-- fromFieldHighPrecMeters ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion HighPrecMeters
-- fromFieldHighPrecMeters f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just _ -> HighPrecMeters <$> fromField f mbValue

-- fromFieldHighPrecMoney ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion HighPrecMoney
-- fromFieldHighPrecMoney f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just _ -> HighPrecMoney <$> fromField f mbValue

-- fromFieldSeconds ::
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion Seconds
-- fromFieldSeconds f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just _ -> Seconds <$> fromField f mbValue

-- instance FromField Minutes where
--   fromField = fromFieldMinutes

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centi where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

-- instance FromBackendRow Postgres Centesimal

-- instance FromField Centesimal where
--   fromField = fromFieldEnum

-- instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [Text] where
--   sqlValueSyntax x = sqlValueSyntax (V.fromList x)

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Text]

-- instance FromBackendRow Postgres [Text]

-- instance FromField [Text] where
--   fromField f mbValue = V.toList <$> fromField f mbValue

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centi

-- instance FromBackendRow Postgres Centi

-- instance HasSqlValueSyntax be Centi => HasSqlValueSyntax be Centesimal where
--   sqlValueSyntax = sqlValueSyntax . getCenti

-- instance HasSqlValueSyntax be Centesimal => HasSqlValueSyntax be HighPrecMeters where
--   sqlValueSyntax = sqlValueSyntax . getHighPrecMeters

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMeters

-- instance FromBackendRow Postgres HighPrecMeters

-- instance FromField HighPrecMeters where
--   fromField = fromFieldHighPrecMeters

-- instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Meters where
--   sqlValueSyntax = sqlValueSyntax . getMeters

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

-- instance FromBackendRow Postgres Meters

-- instance FromField Meters where
--   fromField = fromFieldJSON

-- instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Seconds where
--   sqlValueSyntax = sqlValueSyntax . getSeconds

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

-- instance FromBackendRow Postgres Seconds

-- instance FromField HighPrecMoney where
--   fromField = fromFieldHighPrecMoney

-- instance HasSqlValueSyntax be Rational => HasSqlValueSyntax be HighPrecMoney where
--   sqlValueSyntax = sqlValueSyntax . getHighPrecMoney

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Rational where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMoney

-- instance FromBackendRow Postgres HighPrecMoney

-- instance FromField Seconds where
--   fromField = fromFieldSeconds

-- instance FromField DbHash where
--   fromField = fromFieldEnumDbHash

-- instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be DbHash where
--   sqlValueSyntax = sqlValueSyntax . unDbHash

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

-- instance FromBackendRow Postgres DbHash

instance FromField DomainFP.WaitingCharge where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be DomainFP.WaitingCharge where
  sqlValueSyntax = sqlValueSyntax . encodeToText

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFP.WaitingCharge

instance FromBackendRow Postgres DomainFP.WaitingCharge

instance FromField DomainFP.PlatformFeeCharge where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be DomainFP.PlatformFeeCharge where
  sqlValueSyntax = sqlValueSyntax . encodeToText

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFP.PlatformFeeCharge

instance FromBackendRow Postgres DomainFP.PlatformFeeCharge

instance FromField DomainFP.WaitingChargeInfo where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DomainFP.WaitingChargeInfo where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFP.WaitingChargeInfo

instance FromBackendRow Postgres DomainFP.WaitingChargeInfo

instance FromField DomainFP.NightShiftCharge where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be DomainFP.NightShiftCharge where
  sqlValueSyntax = sqlValueSyntax . encodeToText

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFP.NightShiftCharge

instance FromBackendRow Postgres DomainFP.NightShiftCharge

-- getPoint :: (Double, Double) -> BQ.QGenExpr context Postgres s Point
-- getPoint (lat, lon) = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_SetSRID (ST_Point (" <> show lon <> " , " <> show lat <> "),4326)"))

containsPoint'' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
containsPoint'' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (" <> show lon <> " , " <> show lat <> ")")))

containsPoint' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
containsPoint' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (geom, ST_GeomFromText('POINT (" <> show lon <> " " <> show lat <> ")'))")))

buildRadiusWithin'' :: (Double, Double) -> Int -> BQ.QGenExpr context Postgres s BQ.SqlBool
buildRadiusWithin'' (lat, lon) rad =
  BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_DWithin(point" <> " , " <> getPoint' <> " , " <> show rad <> ")"))
  where
    getPoint' = "(ST_SetSRID (ST_Point (" <> show lon <> " , " <> show lat <> "),4326))"

(<->.) :: (Double, Double) -> BQ.QGenExpr context Postgres s Double
(<->.) (lat, lon) = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "point <-> " <> "ST_SetSRID (ST_Point (" <> show lon <> " , " <> show lat <> "),4326)"))

-- kvTables :: [Text]
-- kvTables = ["registration_token", "search_request", "search_request_for_driver", "search_try", "driver_information", "driver_flow_status", "business_event", "booking", "ride", "estimate", "fare_parameters", "fare_parameters_progressive_details", "booking_location", "ride_details", "rider_details", "driver_stats", "driver_quote", "search_request_location"]

-- kvHardKilledTables :: [Text]
-- kvHardKilledTables = ["registration_token", "search_request", "search_request_for_driver", "search_try", "driver_information", "driver_flow_status", "business_event", "booking", "ride", "estimate", "fare_parameters", "fare_parameters_progressive_details", "booking_location", "ride_details", "rider_details", "driver_stats", "driver_quote", "search_request_location"]

-- setMeshConfig' :: (L.MonadFlow m, HasCallStack) => Text -> MeshConfig -> m MeshConfig
-- setMeshConfig' modelName meshConfig' = do
--   tables <- L.getOption KBT.Tables
--   randomIntV <- L.runIO (randomRIO (1, 100) :: IO Int)
--   case tables of
--     Nothing -> L.throwException $ InternalError "Tables not found"
--     Just tables' -> do
--       let enableKVForWriteAlso = tables'.enableKVForWriteAlso
--       let enableKVForRead = tables'.enableKVForRead
--       let tableAllocation = fromIntegral tables'.tableAllocation
--       if randomIntV <= tableAllocation
--         then pure $ meshConfig' {meshEnabled = modelName `elem` enableKVForWriteAlso, kvHardKilled = modelName `notElem` enableKVForRead}
--         else pure $ meshConfig' {meshEnabled = False, kvHardKilled = modelName `notElem` enableKVForRead}

-- data ReplicaEnabled = ReplicaEnabled
--   deriving stock (Generic, Typeable, Show, Eq)
--   deriving anyclass (ToJSON, FromJSON)

-- instance OptionEntity ReplicaEnabled Bool

-- runInReplica :: MonadFlow m => m a -> m a
-- runInReplica m = do
--   L.setOption ReplicaEnabled True
--   res <- m
--   L.setOption ReplicaEnabled False
--   pure res

-- class
--   FromTType' t a
--     | t -> a
--   where
--   fromTType' :: (MonadThrow m, Log m, L.MonadFlow m) => t -> m (Maybe a)

-- class
--   ToTType' t a
--     | a -> t
--   where
--   toTType' :: a -> t

-- findOneWithKV ::
--   forall table m a.
--   ( HasCallStack,
--     FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   -- DBConfig beM ->
--   Where Postgres table ->
--   m (Maybe a)
-- -- m (Maybe (table Identity))
-- findOneWithKV where' = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   inReplica <- L.getOption ReplicaEnabled
--   dbConf' <- maybe getMasterDBConfig (\inReplica' -> if inReplica' then getReplicaDbConfig else getMasterDBConfig) inReplica
--   result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
--   case result of
--     Right (Just res) -> fromTType' res
--     Right Nothing -> pure Nothing
--     Left err -> throwError $ InternalError $ show err

-- findAllWithKV ::
--   forall table m a.
--   ( HasCallStack,
--     FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   -- DBConfig beM ->
--   Where Postgres table ->
--   m [a]
-- -- m (Maybe (table Identity))
-- findAllWithKV where' = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf' <- getMasterDBConfig
--   result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig where'
--   case result of
--     Right res -> do
--       res' <- mapM fromTType' res
--       pure $ catMaybes res'
--     Left err -> throwError $ InternalError $ show err

-- findAllWithOptionsKV ::
--   forall table m a.
--   ( HasCallStack,
--     FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   -- DBConfig beM ->
--   Where Postgres table ->
--   OrderBy table ->
--   Maybe Int ->
--   Maybe Int ->
--   m [a]
-- -- m (Maybe (table Identity))
-- findAllWithOptionsKV where' orderBy mbLimit mbOffset = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf <- getMasterDBConfig
--   result <- KV.findAllWithOptionsKVConnector dbConf updatedMeshConfig where' orderBy mbLimit mbOffset
--   case result of
--     Right res -> do
--       res' <- mapM fromTType' res
--       pure $ catMaybes res'
--     Left err -> throwError $ InternalError $ show err

-- updateWithKV ::
--   forall table m.
--   ( HasCallStack,
--     -- FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     SqlReturning Pg Postgres,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   -- DBConfig beM ->throwError
--   [Set Postgres table] ->
--   Where Postgres table ->
--   m ()
-- -- m (Maybe (table Identity))
-- updateWithKV setClause whereClause = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf <- getMasterDBConfig
--   res <- KV.updateAllWithKVConnector dbConf updatedMeshConfig setClause whereClause
--   case res of
--     Right res' -> do
--       if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
--         then logDebug $ "Updated rows KV: " <> show res'
--         else logDebug $ "Updated rows DB: " <> show res'
--       pure ()
--     Left err -> throwError $ InternalError $ show err

-- updateOneWithKV ::
--   forall table m.
--   ( HasCallStack,
--     -- FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     SqlReturning Pg Postgres,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   -- DBConfig beM ->throwError
--   [Set Postgres table] ->
--   Where Postgres table ->
--   m ()
-- -- m (Maybe (table Identity))
-- updateOneWithKV setClause whereClause = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf <- getMasterDBConfig
--   res <- KV.updateWoReturningWithKVConnector dbConf updatedMeshConfig setClause whereClause
--   case res of
--     Right _ -> pure ()
--     Left err -> throwError $ InternalError $ show err

-- createWithKV ::
--   forall table m a.
--   ( HasCallStack,
--     ToTType' (table Identity) a,
--     SqlReturning Pg Postgres,
--     BeamRuntime Postgres Pg,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   -- DBConfig Pg ->
--   a ->
--   m ()
-- createWithKV a = do
--   let tType = toTType' a
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf' <- getMasterDBConfig
--   result <- KV.createWoReturingKVConnector dbConf' updatedMeshConfig tType
--   case result of
--     Right _ -> do
--       if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
--         then logDebug $ "Created row in KV: " <> show tType
--         else logDebug $ "Created row in DB: " <> show tType
--       pure ()
--     Left err -> throwError $ InternalError $ show err

-- deleteWithKV ::
--   forall be table beM m.
--   ( HasCallStack,
--     BeamRuntime be beM,
--     SqlReturning beM be,
--     B.HasQBuilder be,
--     BeamRunner beM,
--     Model be table,
--     MeshMeta be table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Log m,
--     Show (table Identity),
--     MonadThrow m,
--     SqlReturning Pg be,
--     BeamRuntime be Pg
--   ) =>
--   Where be table ->
--   m ()
-- deleteWithKV whereClause = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf <- getMasterDBConfig
--   res <- KV.deleteAllReturningWithKVConnector dbConf updatedMeshConfig whereClause
--   case res of
--     Right _ -> do
--       if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
--         then logDebug $ "Deleted rows in KV: " <> show res
--         else logDebug $ "Deleted rows in DB: " <> show res
--       pure ()
--     Left err -> throwError $ InternalError $ show err

-- getMasterDBConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
-- getMasterDBConfig = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   case dbConf of
--     Just dbCnf' -> pure dbCnf'
--     Nothing -> L.throwException $ InternalError "DB Config not found"

-- getLocDbConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
-- getLocDbConfig = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   case dbConf of
--     Just dbCnf' -> pure dbCnf'
--     Nothing -> L.throwException $ InternalError "DB Config not found"

-- getMasterBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
-- getMasterBeamConfig = do
--   dbConf <- getMasterDBConfig
--   conn <- L.getOrInitSqlConn dbConf
--   case conn of
--     Right conn' -> pure conn'
--     Left _ -> L.throwException $ InternalError "DB Config not found"

-- getLocationDbBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
-- getLocationDbBeamConfig = do
--   dbConf <- getLocDbConfig
--   conn <- L.getOrInitSqlConn dbConf
--   case conn of
--     Right conn' -> pure conn'
--     Left _ -> L.throwException $ InternalError "DB Config not found"

-- ----- replica db funcitons---------------

-- getReplicaDbConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
-- getReplicaDbConfig = do
--   dbConf <- L.getOption KBT.PsqlDbCfgR1
--   case dbConf of
--     Just dbCnf' -> pure dbCnf'
--     Nothing -> L.throwException $ InternalError "DB Config not found"

-- getReplicaLocationDbConfig :: (HasCallStack, L.MonadFlow m) => m (DBConfig Pg)
-- getReplicaLocationDbConfig = do
--   dbConf <- L.getOption KBT.PsqlDbCfgR1
--   case dbConf of
--     Just dbCnf' -> pure dbCnf'
--     Nothing -> L.throwException $ InternalError "DB Config not found"

-- getReplicaBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
-- getReplicaBeamConfig = do
--   dbConf <- getReplicaDbConfig
--   conn <- L.getOrInitSqlConn dbConf
--   case conn of
--     Right conn' -> pure conn'
--     Left _ -> L.throwException $ InternalError "DB Config not found"

-- getReplicaLocationDbBeamConfig :: (HasCallStack, L.MonadFlow m) => m (SqlConn Pg)
-- getReplicaLocationDbBeamConfig = do
--   dbConf <- getReplicaLocationDbConfig
--   conn <- L.getOrInitSqlConn dbConf
--   case conn of
--     Right conn' -> pure conn'
--     Left _ -> L.throwException $ InternalError "DB Config not found"

-- findAllWithKvInReplica ::
--   forall table m a.
--   ( HasCallStack,
--     FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   Where Postgres table ->
--   m [a]
-- findAllWithKvInReplica where' = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf <- getReplicaDbConfig
--   result <- KV.findAllWithKVConnector dbConf updatedMeshConfig where'
--   case result of
--     Right res -> do
--       res' <- mapM fromTType' res
--       pure $ catMaybes res'
--     Left err -> throwError $ InternalError $ show err

-- findAllWithOptionsKvInReplica ::
--   forall table m a.
--   ( HasCallStack,
--     FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   Where Postgres table ->
--   OrderBy table ->
--   Maybe Int ->
--   Maybe Int ->
--   m [a]
-- findAllWithOptionsKvInReplica where' orderBy mbLimit mbOffset = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf <- getReplicaDbConfig
--   result <- KV.findAllWithOptionsKVConnector dbConf updatedMeshConfig where' orderBy mbLimit mbOffset
--   case result of
--     Right res -> do
--       res' <- mapM fromTType' res
--       pure $ catMaybes res'
--     Left err -> throwError $ InternalError $ show err

-- findOneWithKvInReplica ::
--   forall table m a.
--   ( HasCallStack,
--     FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   Where Postgres table ->
--   m (Maybe a)
-- findOneWithKvInReplica where' = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf' <- getReplicaDbConfig
--   result <- KV.findWithKVConnector dbConf' updatedMeshConfig where'
--   case result of
--     Right (Just res) -> fromTType' res
--     Right Nothing -> pure Nothing
--     Left err -> throwError $ InternalError $ show err

-- updateWithKvInReplica ::
--   forall table m.
--   ( HasCallStack,
--     -- FromTType' (table Identity) a,
--     BeamRuntime Postgres Pg,
--     SqlReturning Pg Postgres,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   [Set Postgres table] ->
--   Where Postgres table ->
--   m ()
-- updateWithKvInReplica setClause whereClause = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf <- getReplicaDbConfig
--   res <- KV.updateAllWithKVConnector dbConf updatedMeshConfig setClause whereClause
--   case res of
--     Right res' -> do
--       if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
--         then logDebug $ "Updated rows in replica with KV: " <> show res'
--         else logDebug $ "Updated rows in replica with DB: " <> show res'
--       pure ()
--     Left err -> throwError $ InternalError $ show err

-- createWithKvInReplica ::
--   forall table m a.
--   ( HasCallStack,
--     ToTType' (table Identity) a,
--     SqlReturning Pg Postgres,
--     BeamRuntime Postgres Pg,
--     B.HasQBuilder Postgres,
--     BeamRunner Pg,
--     Model Postgres table,
--     MeshMeta Postgres table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Show (table Identity),
--     Log m,
--     MonadThrow m
--   ) =>
--   a ->
--   m ()
-- createWithKvInReplica a = do
--   let tType = toTType' a
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf' <- getReplicaDbConfig
--   result <- KV.createWoReturingKVConnector dbConf' updatedMeshConfig tType
--   case result of
--     Right _ -> do
--       if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
--         then logDebug $ "Created row in KV" <> show tType
--         else logDebug $ "Created row in DB" <> show tType
--       pure ()
--     Left err -> throwError $ InternalError $ show err

-- deleteWithKvInReplica ::
--   forall be table beM m.
--   ( HasCallStack,
--     BeamRuntime be beM,
--     SqlReturning beM be,
--     B.HasQBuilder be,
--     BeamRunner beM,
--     Model be table,
--     MeshMeta be table,
--     KVConnector (table Identity),
--     FromJSON (table Identity),
--     ToJSON (table Identity),
--     Serialize.Serialize (table Identity),
--     L.MonadFlow m,
--     Log m,
--     Show (table Identity),
--     MonadThrow m,
--     SqlReturning Pg be,
--     BeamRuntime be Pg
--   ) =>
--   Where be table ->
--   m ()
-- deleteWithKvInReplica whereClause = do
--   updatedMeshConfig <- setMeshConfig' (modelTableName @table) meshConfig
--   dbConf <- getReplicaDbConfig
--   res <- KV.deleteAllReturningWithKVConnector dbConf updatedMeshConfig whereClause
--   case res of
--     Right _ -> do
--       if updatedMeshConfig.meshEnabled && not updatedMeshConfig.kvHardKilled
--         then logDebug $ "Deleted rows in KV: " <> show res
--         else logDebug $ "Deleted rows in DB: " <> show res
--       pure ()
--     Left err -> throwError $ InternalError $ show err
