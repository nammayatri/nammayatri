{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.WalkLegMultimodal where

import Data.Aeson
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.JourneyLeg.Types
import qualified Tools.Beam.UtilsTH

data WalkLegMultimodal = WalkLegMultimodal
  { estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromLocation :: Domain.Types.Location.Location,
    id :: Kernel.Types.Id.Id Domain.Types.WalkLegMultimodal.WalkLegMultimodal,
    journeyLegInfo :: Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneySearchData,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.WalkLegMultimodal.WalkLegStatus,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data WalkLegStatus = InPlan | Ongoing | Finishing | Completed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''WalkLegStatus)
