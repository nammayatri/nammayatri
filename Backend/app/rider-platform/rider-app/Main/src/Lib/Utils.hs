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
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.KVConnector.Types (MeshConfig (..))
import qualified EulerHS.Language as L
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
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

kvTables :: [Text]
kvTables = []

kvHardKilledTables :: [Text]
kvHardKilledTables = []

setMeshConfig :: Text -> MeshConfig
setMeshConfig modelTableName = meshConfig {meshEnabled = modelTableName `elem` kvTables, kvHardKilled = modelTableName `notElem` kvHardKilledTables}
