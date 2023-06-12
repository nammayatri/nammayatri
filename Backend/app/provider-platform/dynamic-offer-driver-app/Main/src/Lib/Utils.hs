{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Utils where

-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Builder as TL

-- import Database.Esqueleto.Internal.Internal hiding (rand)

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import Data.ByteString.Lazy (fromStrict)
import Data.Fixed (Centi)
import Data.Time
import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.FarePolicy as DomainFP
import Domain.Types.Vehicle.Variant (Variant (..))
import EulerHS.KVConnector.Types (MeshConfig (..))
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.Common
import Kernel.Utils.Common (encodeToText)
import Lib.Mesh as Mesh

-- import Kernel.Types.Time (Seconds (..))

-- checkContains :: BeamSqlBackend be => a -> b -> QExpr be s Bool
-- checkContains polygonGeometry pointGeometry =
--   function "ST_Contains" (polygonGeometry, pointGeometry)

defaultDate :: LocalTime
defaultDate =
  LocalTime
    { localDay = toEnum 1, --   :: Day,
      localTimeOfDay = defaultTimeOfDay --  :: TimeOfDay
    }

defaultTimeOfDay :: TimeOfDay
defaultTimeOfDay =
  TimeOfDay
    { todHour = 1, -- :: Int,-  range 0 - 23
      todMin = 1, -- :: Int, --  range 0 - 59
      -- Note that 0 <= 'todSec' < 61, accomodating leap seconds.
      -- Any local minute may have a leap second, since leap seconds happen in all zones simultaneously
      todSec = 1 -- :: Pico, type Pico = Fixed E12
    }

defaultUTCDate :: UTCTime
defaultUTCDate = localTimeToUTC utc defaultDate

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
--   sqlValueSyntax = autoSqlValueSyntax

-- fromFieldJSON ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldJSON f mbValue = do
--   value <- fromField f mbValue
--   case A.fromJSON value of
--     A.Success a -> pure a
--     _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Money where
  sqlValueSyntax = sqlValueSyntax . getMoney

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

instance FromField Money where
  fromField = fromFieldMoney

instance FromField Centi where
  fromField = fromFieldCenti

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Minutes where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Minutes

instance FromBackendRow Postgres Minutes

fromFieldMoney ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Money
fromFieldMoney f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Money <$> (fromField f mbValue)

fromFieldCenti ::
  -- (Typeable a, Read a) =>
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
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Centesimal
fromFieldCentesimal f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Centesimal <$> (fromField f mbValue)

fromFieldMinutes ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Minutes
fromFieldMinutes f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Minutes <$> (fromField f mbValue)

fromFieldMeters ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Meters
fromFieldMeters f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Meters <$> (fromField f mbValue)

fromFieldHighPrecMeters ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion HighPrecMeters
fromFieldHighPrecMeters f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> HighPrecMeters <$> (fromField f mbValue)

fromFieldHighPrecMoney ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion HighPrecMoney
fromFieldHighPrecMoney f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> HighPrecMoney <$> (fromField f mbValue)

fromFieldSeconds ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion Seconds
fromFieldSeconds f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just _ -> Seconds <$> (fromField f mbValue)

instance FromField Minutes where
  fromField = fromFieldMinutes

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centi where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

instance FromField Centesimal where
  fromField = fromFieldEnum

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centi

instance FromBackendRow Postgres Centi

instance HasSqlValueSyntax be Centi => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = sqlValueSyntax . getCenti

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Variant

instance FromBackendRow Postgres Variant

instance FromField Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be Centesimal => HasSqlValueSyntax be HighPrecMeters where
  sqlValueSyntax = sqlValueSyntax . getHighPrecMeters

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMeters

instance FromBackendRow Postgres HighPrecMeters

instance FromField HighPrecMeters where
  fromField = fromFieldHighPrecMeters

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Meters where
  sqlValueSyntax = sqlValueSyntax . getMeters

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Meters where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = sqlValueSyntax . getSeconds

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance FromField HighPrecMoney where
  fromField = fromFieldHighPrecMoney

instance HasSqlValueSyntax be Rational => HasSqlValueSyntax be HighPrecMoney where
  sqlValueSyntax = sqlValueSyntax . getHighPrecMoney

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Rational where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMoney

instance FromBackendRow Postgres HighPrecMoney

instance FromField Seconds where
  fromField = fromFieldSeconds

instance FromField DbHash where
  fromField = fromFieldEnumDbHash

instance HasSqlValueSyntax be ByteString => HasSqlValueSyntax be DbHash where
  sqlValueSyntax = sqlValueSyntax . unDbHash

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

instance FromBackendRow Postgres DbHash

instance IsString DbHash where
  fromString = show

instance FromField DomainFP.WaitingCharge where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be DomainFP.WaitingCharge where
  sqlValueSyntax = sqlValueSyntax . encodeToText

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFP.WaitingCharge

instance FromBackendRow Postgres DomainFP.WaitingCharge

instance FromField DomainFP.WaitingChargeInfo where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DomainFP.WaitingChargeInfo where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFP.WaitingChargeInfo

instance FromBackendRow Postgres DomainFP.WaitingChargeInfo

fromFieldJSON ::
  (Typeable a, Read a, FromJSON a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldJSON f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' -> case A.decode $ fromStrict value' of
    Just res -> pure res
    Nothing -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField DomainFP.NightShiftCharge where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be DomainFP.NightShiftCharge where
  sqlValueSyntax = sqlValueSyntax . encodeToText

-- sqlValueSyntax = sqlValueSyntax . encodeToText

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DomainFP.NightShiftCharge

instance FromBackendRow Postgres DomainFP.NightShiftCharge

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case readMaybe (unpackChars value') of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

fromFieldEnumDbHash ::
  -- (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion DbHash
fromFieldEnumDbHash f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' -> pure $ DbHash value'

-- writing shared kernel Esqueleto.Functions in utils

-- getPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value Point)
-- getPoint (lat, long) = unsafeSqlFunction "ST_SetSRID" (buildSTPoint (long, lat), val (4326 :: Int))

getPoint :: (Double, Double) -> BQ.QGenExpr context Postgres s Point
getPoint (lat, lon) = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_SetSRID (ST_Point (" <> show lon <> " , " <> show lat <> "),4326)"))

containsPoint'' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
containsPoint'' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (" <> show lon <> " , " <> show lat <> ")")))

containsPoint' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
containsPoint' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (geom, ST_GeomFromText('POINT (" <> show lon <> " " <> show lat <> ")'))")))

buildRadiusWithin' :: Point -> (Double, Double) -> Int -> BQ.QGenExpr context Postgres s BQ.SqlBool
buildRadiusWithin' pnt (lat, lon) rad =
  BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_DWithin(" <> show pnt <> " , " <> getPoint' <> " , " <> show rad <> ")"))
  where
    getPoint' = "(SRID=4326;POINT(" <> show lon <> " " <> show lat <> "))"

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

-- " <-> "
