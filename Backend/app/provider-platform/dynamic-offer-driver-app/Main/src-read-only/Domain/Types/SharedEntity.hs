{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SharedEntity where

import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TrackedEntity
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SharedEntity = SharedEntity
  { bookingIds :: Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity],
    counterAppSharedEntityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    driverQuoteIds :: Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity],
    entityType :: Domain.Types.SharedEntity.SharedEntityType,
    estimateIds :: Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity],
    id :: Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    pairingTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rideIds :: Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity],
    searchRequestForDriverIds :: Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity],
    searchRequestIds :: Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity],
    status :: Domain.Types.SharedEntity.SharedEntityStatus,
    totalSeats :: Kernel.Prelude.Int,
    transactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleCategory :: BecknV2.OnDemand.Enums.VehicleCategory,
    waypoints :: Data.Aeson.Value
  }
  deriving (Generic, Show)

data SharedEntityStatus
  = SEARCHING
  | MATCHED
  | ESTIMATED
  | OFFERED_QUOTE
  | BOOKED
  | DRIVER_ASSIGNED
  | ONGOING
  | COMPLETED
  | CANCELLED
  | EXPIRED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SharedEntityType = SEARCH_GROUP | ESTIMATE_GROUP | BOOKING_GROUP | RIDE_GROUP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SharedEntityStatus)

$(mkHttpInstancesForEnum ''SharedEntityStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SharedEntityType)

$(mkHttpInstancesForEnum ''SharedEntityType)
