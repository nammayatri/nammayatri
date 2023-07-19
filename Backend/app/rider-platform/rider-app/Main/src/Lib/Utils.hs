{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Utils where

import Data.ByteString.Internal (ByteString)
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
import qualified Domain.Types.FarePolicy.FareProductType as DomainFPT
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.KVConnector.Types (MeshConfig (..))
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Lib.Mesh as Mesh

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

instance FromField DomainFPT.FareProductType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DomainFPT.FareProductType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFPT.FareProductType

instance FromBackendRow Postgres DomainFPT.FareProductType

instance IsString DomainFPT.FareProductType where
  fromString = show

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

-- kvTables :: [Text]
-- kvTables = [] -- ["app_installs", "black_list_org", "booking", "booking_cancellation_reason", "callback_request", "call_status", "cancellation_reason", "driver_offer", "estimate", "estimate_breakup", "exophone", "geometry", "issue", "merchant", "merchant_config", "on_search_event", "person", "quote", "registration_token", "rental_slab", "ride", "saved_location", "search_request", "sos", "special_zone_quote", "trip_terms", "webengage", "booking_location", "fare_breakup", "place_name_cache", "merchant_message", "merchant_payment_method", "merchant_service_config", "merchant_service_usage_config", "payment_order", "payment_transaction", "person_default_emergency_number", "person_flow_status", "search_request_location"]

-- kvHardKilledTables :: [Text]
-- kvHardKilledTables = [] -- ["app_installs", "black_list_org", "booking", "booking_cancellation_reason", "callback_request", "call_status", "cancellation_reason", "driver_offer", "estimate", "estimate_breakup", "exophone", "geometry", "issue", "merchant", "merchant_config", "on_search_event", "person", "quote", "registration_token", "rental_slab", "ride", "saved_location", "search_request", "sos", "special_zone_quote", "trip_terms", "webengage", "booking_location", "fare_breakup", "place_name_cache", "merchant_message", "merchant_payment_method", "merchant_service_config", "merchant_service_usage_config", "payment_order", "payment_transaction", "person_default_emergency_number", "person_flow_status", "search_request_location"]

setMeshConfig :: (L.MonadFlow m, HasCallStack) => Text -> m MeshConfig
setMeshConfig modelName = do
  tables <- L.getOption KBT.Tables
  case tables of
    Nothing -> L.throwException $ InternalError "Tables not found"
    Just tables' -> do
      let kvTables = tables'.kVTables
      let kvHardKilledTables = tables'.kVHardKilledTables
      pure $ meshConfig {meshEnabled = modelName `elem` kvTables, kvHardKilled = modelName `notElem` kvHardKilledTables}
