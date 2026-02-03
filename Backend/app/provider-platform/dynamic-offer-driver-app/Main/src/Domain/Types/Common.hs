{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Common
  ( module Domain.Types.Common,
    module Domain.Types,
    module Data.Time.Calendar,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import Data.Aeson
import Data.Time.Calendar
import Domain.Types
import qualified Domain.Types.Alert.AlertRequestData as DTAAD
import qualified Domain.Types.Alert.AlertRequestStatus as DTAAS
import qualified Domain.Types.Alert.AlertRequestType as DTAAT
import qualified Domain.Types.FleetBadgeType as DFBT
import qualified Domain.Types.ParcelType as DTPT
import qualified Domain.Types.VehicleCategory as DTVC
import qualified Domain.Types.VehicleVariant as DTVV
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Utils.TH
import Tools.Beam.UtilsTH

$(mkBeamInstancesForEnumAndList ''ServiceTierType)
$(mkBeamInstancesForEnumAndList ''DTVV.VehicleVariant)
$(mkBeamInstancesForEnumAndList ''DTVC.VehicleCategory)
$(mkBeamInstancesForEnumAndList ''Enums.VehicleCategory)
$(mkBeamInstancesForEnum ''TripCategory)
$(mkBeamInstancesForEnum ''TripParty)
$(mkBeamInstancesForEnum ''DTAAT.AlertRequestType)
$(mkHttpInstancesForEnum ''DTAAT.AlertRequestType)
$(mkBeamInstancesForEnum ''DTAAS.AlertRequestStatus)
$(mkHttpInstancesForEnum ''DTAAS.AlertRequestStatus)
$(mkBeamInstancesForEnumAndList ''DTAAD.AlertRequestData)
$(mkFromHttpInstanceForEnum ''DTAAD.AlertRequestData)
$(mkBeamInstancesForEnumAndList ''DFBT.FleetBadgeType)
$(mkHttpInstancesForEnum ''DFBT.FleetBadgeType)
$(mkBeamInstancesForEnumAndList ''DTPT.ParcelType)

data DriverMode = ONLINE | OFFLINE | SILENT
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance CH.ClickhouseValue DriverMode

data SearchRequestForDriverResponse = Accept | Reject | Pulled
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance CH.ClickhouseValue SearchRequestForDriverResponse

$(mkHttpInstancesForEnum ''SearchRequestForDriverResponse)

$(mkBeamInstancesForEnumAndList ''SearchRequestForDriverResponse)

$(mkHttpInstancesForEnum ''DriverMode)

$(mkBeamInstancesForEnumAndList ''DriverMode)
$(mkBeamInstancesForEnum ''DayOfWeek)

data MediaFileDocumentType = VehicleVideo
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToSchema, ToParamSchema)

-- Type with single constructor will be serialized as empty list by default, that's why manual instances required
instance ToJSON MediaFileDocumentType where
  toJSON VehicleVideo = String "VehicleVideo"

instance FromJSON MediaFileDocumentType where
  parseJSON (String "VehicleVideo") = pure VehicleVideo
  parseJSON _ = fail "VehicleVideo expected"

$(mkBeamInstancesForEnumAndList ''MediaFileDocumentType)

$(mkHttpInstancesForEnum ''MediaFileDocumentType)
