{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TripAlertRequest where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.AlertRequest
import qualified Domain.Types.FleetBadge
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TripTransaction
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data TripAlertRequest = TripAlertRequest
  { alertRequestId :: Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest,
    alertRequestType :: Domain.Types.Alert.AlertRequestType.AlertRequestType,
    alertStatus :: Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus,
    conductorFleetBadgeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge),
    createdAt :: Kernel.Prelude.UTCTime,
    driverFleetBadgeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge),
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.TripAlertRequest.TripAlertRequest,
    isViolated :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    routeCode :: Data.Text.Text,
    tripTransactionId :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
