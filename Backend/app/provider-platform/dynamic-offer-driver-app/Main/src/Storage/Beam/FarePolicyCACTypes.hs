{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.FarePolicyCACTypes where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)

data FarePolicyProgressiveDetailsPerExtraKmRateSection = FarePolicyProgressiveDetailsPerExtraKmRateSection
  { startDistance :: Meters,
    perExtraKmRate :: HighPrecMoney
  }
  deriving (Generic, FromJSON, ToJSON)

data FarePolicyRentalDetailsDistanceBuffers = FarePolicyRentalDetailsDistanceBuffers
  { farePolicyId :: Text,
    rideDuration :: Seconds,
    bufferKms :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

data FarePolicySlabsDetailsSlab = FarePolicySlabsDetailsSlab
  { startDistance :: Meters,
    baseFare :: Money,
    platformFeeCharge :: Maybe Domain.PlatformFeeCharge,
    platformFeeCgst :: Maybe Double,
    platformFeeSgst :: Maybe Double,
    waitingCharge :: Maybe DPM.WaitingCharge,
    freeWatingTime :: Maybe Minutes, -- FIXME typo
    nightShiftCharge :: Maybe DPM.NightShiftCharge
  }
  deriving (Generic, FromJSON, ToJSON)

$(mkCacParseInstanceList ''FarePolicyProgressiveDetailsPerExtraKmRateSection "farePolicyProgressiveDetailsPerExtraKmRateSection")
$(mkCacParseInstanceList ''FarePolicyRentalDetailsDistanceBuffers "farePolicyRentalDetailsDistanceBuffers")
$(mkCacParseInstanceList ''FarePolicySlabsDetailsSlab "farePolicySlabsDetailsSlab")
