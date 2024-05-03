{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.FarePolicyCACTypes where

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
  { rideDuration :: Seconds,
    bufferKms :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

data FarePolicySlabsDetailsSlab = FarePolicySlabsDetailsSlab
  { startDistance :: Meters,
    baseFare :: Money,
    baseFareAmount :: Maybe HighPrecMoney,
    currency :: Maybe Currency,
    platformFeeCharge :: Maybe Domain.PlatformFeeCharge,
    platformFeeCgst :: Maybe Double,
    platformFeeSgst :: Maybe Double,
    waitingCharge :: Maybe Domain.WaitingCharge,
    freeWatingTime :: Maybe Minutes, -- FIXME typo
    nightShiftCharge :: Maybe Domain.NightShiftCharge
  }
  deriving (Generic, FromJSON, ToJSON)

$(mkCacParseInstanceList ''FarePolicyProgressiveDetailsPerExtraKmRateSection)
$(mkCacParseInstanceList ''FarePolicyRentalDetailsDistanceBuffers)
$(mkCacParseInstanceList ''FarePolicySlabsDetailsSlab)
