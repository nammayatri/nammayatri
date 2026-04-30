{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SvpJourney where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SvpJourney = SvpJourney
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    entryStationCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entryTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    exitStationCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    exitTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fareCharged :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.SvpJourney.SvpJourney,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    status :: Domain.Types.SvpJourney.SvpJourneyStatus,
    tktSlNo :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SvpJourneyStatus = ENTERED | EXITED | TIMED_OUT deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (Kernel.Prelude.ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''SvpJourneyStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''SvpJourneyStatus))

$(Kernel.Utils.TH.mkToHttpInstanceForEnum (''SvpJourneyStatus))
