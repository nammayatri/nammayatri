{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Utils where

import Data.ByteString.Internal (ByteString, unpackChars)
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Builder as TL
import Data.Time
-- import Database.Esqueleto.Internal.Internal hiding (rand)

import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query as BQ
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.Common

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

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Variant

instance FromBackendRow Postgres Variant

instance FromField Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be HighPrecMeters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMeters

instance FromBackendRow Postgres HighPrecMeters

instance FromField HighPrecMeters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance FromField HighPrecMoney where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be HighPrecMoney where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be HighPrecMoney

instance FromBackendRow Postgres HighPrecMoney

instance FromField Seconds where
  fromField = fromFieldEnum

instance FromField DbHash where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DbHash where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DbHash

instance FromBackendRow Postgres DbHash

instance IsString DbHash where
  fromString = show

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

-- writing shared kernel Esqueleto.Functions in utils

getPoint :: (Double, Double) -> BQ.QGenExpr context Postgres s Point
getPoint (lat, lon) = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_SetSRID (ST_Point (" <> show lon <> " , " <> show lat <> "),4326)"))

containsPoint' :: (Double, Double) -> BQ.QGenExpr context Postgres s BQ.SqlBool
containsPoint' (lon, lat) = B.sqlBool_ (BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "st_contains (" <> show lon <> " , " <> show lat <> ")")))

buildRadiusWithin' :: Point -> (Double, Double) -> Int -> BQ.QGenExpr context Postgres s BQ.SqlBool
buildRadiusWithin' pnt (lat, lon) rad =
  BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "ST_DWithin(" <> show pnt <> " , " <> getPoint' <> " , " <> show rad <> ")"))
  where
    getPoint' = "(SRID=4326;POINT(" <> show lon <> " " <> show lat <> "))"

-- (<->.) :: Point -> Point -> Double
-- (<->.) = " <-> "
