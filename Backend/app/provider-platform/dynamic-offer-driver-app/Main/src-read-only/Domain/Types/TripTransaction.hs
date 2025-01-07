{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TripTransaction where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data TripTransaction = TripTransaction
  { allowEndingMidRoute :: Kernel.Prelude.Bool,
    deviationCount :: Kernel.Prelude.Int,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    endLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    endStopCode :: Data.Text.Text,
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction,
    isCurrentlyDeviated :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    routeCode :: Data.Text.Text,
    startLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    startedNearStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    status :: Domain.Types.TripTransaction.TripStatus,
    tripCode :: Kernel.Prelude.Maybe Data.Text.Text,
    tripEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleNumber :: Data.Text.Text,
    vehicleServiceTierType :: Domain.Types.Common.ServiceTierType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TripStatus = TRIP_ASSIGNED | IN_PROGRESS | PAUSED | COMPLETED | CANCELLED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''TripStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''TripStatus))
