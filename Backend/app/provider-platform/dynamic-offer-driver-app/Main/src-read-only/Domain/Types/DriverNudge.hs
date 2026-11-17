{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverNudge where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data DriverNudge = DriverNudge
  { actualDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    createdAt :: Kernel.Prelude.UTCTime,
    destination :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    id :: Kernel.Types.Id.Id Domain.Types.DriverNudge.DriverNudge,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    status :: Domain.Types.DriverNudge.NudgeStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data NudgeStatus = PENDING | ACTIVE | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''NudgeStatus)

$(mkHttpInstancesForEnum ''NudgeStatus)
