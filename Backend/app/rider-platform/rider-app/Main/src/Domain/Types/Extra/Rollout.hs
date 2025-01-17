{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.Rollout where

import Data.Aeson
import Data.Aeson.Types
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON (constructorsToLowerOptions, constructorsWithHyphens)
import Kernel.Utils.TH

-- Extra code goes here --

fromFieldRawDataType ::
  Field ->
  Maybe ByteString ->
  Conversion RawDataType
fromFieldRawDataType f mbValue = do
  value <- fromField f mbValue
  case fromJSON value of
    Success a -> pure a
    _ -> returnError ConversionFailed f "Conversion failed"

data RawDataType = GTFS | FARE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''RawDataType)

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be RawDataType where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be RawDataType

instance FromField RawDataType where
  fromField = fromFieldRawDataType

instance FromBackendRow Postgres RawDataType

fromFieldStageData ::
  Field ->
  Maybe ByteString ->
  Conversion StageData
fromFieldStageData f mbValue = do
  value <- fromField f mbValue
  case fromJSON value of
    Success a -> pure a
    _ -> returnError ConversionFailed f "Conversion failed"

data GtfsDataType = GtfsDataType
  { id :: Text
  }
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

-- instance HasSqlValueSyntax be Value => HasSqlValueSyntax be GtfsDataType where
--   sqlValueSyntax = sqlValueSyntax . toJSON

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be GtfsDataType

-- instance FromField GtfsDataType where
--   fromField = fromFieldStageData

-- instance FromBackendRow Postgres GtfsDataType

data FareDataType = FareDataType
  { id :: Text
  }
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

-- instance HasSqlValueSyntax be Value => HasSqlValueSyntax be FareDataType where
--   sqlValueSyntax = sqlValueSyntax . toJSON

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be FareDataType

-- instance FromField FareDataType where
--   fromField = fromFieldStageData

-- instance FromBackendRow Postgres FareDataType

data StageData = GTFSData GtfsDataType | FAREData FareDataType
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''StageData)

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be StageData where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be StageData

instance FromField StageData where
  fromField = fromFieldStageData

instance FromBackendRow Postgres StageData
