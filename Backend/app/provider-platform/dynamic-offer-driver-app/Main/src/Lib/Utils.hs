{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Utils where

import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Time
-- import Database.Esqueleto.Internal.Internal hiding (rand)

import Database.Beam
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
import qualified Database.Esqueleto.Experimental as Esq
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Types
import Kernel.Types.Common
import Kernel.Types.Time (Seconds (..))

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

instance FromField Seconds where
  fromField = fromFieldEnum

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

-- (<->.) :: SqlExpr (Value Point) -> SqlExpr (Value Point) -> SqlExpr (Value Double)
-- (<->.) = unsafeSqlBinOp " <-> "

-- getPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value Point)
-- getPoint (lat, long) = unsafeSqlFunction "ST_SetSRID" (buildSTPoint (long, lat), val (4326 :: Int))

-- buildSTPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value b)
-- buildSTPoint = unsafeSqlFunction "ST_Point"
