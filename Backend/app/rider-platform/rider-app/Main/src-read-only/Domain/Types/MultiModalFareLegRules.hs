{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MultiModalFareLegRules where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultiModalNetwork
import qualified Domain.Types.MultiModalTimeFrame
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MultiModalFareLegRules = MultiModalFareLegRules
  { amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    fromTimeFrameId :: Kernel.Types.Id.Id Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame,
    id :: Kernel.Types.Id.Id Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules,
    maxDist :: Kernel.Types.Common.Meters,
    minDist :: Kernel.Types.Common.Meters,
    networkId :: Kernel.Types.Id.Id Domain.Types.MultiModalNetwork.MultiModalNetwork,
    passengerType :: Domain.Types.MultiModalFareLegRules.MultiModalPassengerType,
    paymentMedia :: Domain.Types.MultiModalFareLegRules.MultiModalFareMediaType,
    toTimeFrameId :: Kernel.Types.Id.Id Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Eq))

data MultiModalFareMediaType = Cash | TransitCard | PrePaid deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data MultiModalPassengerType = Adult | Child | SeniorCitizen deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''MultiModalFareMediaType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''MultiModalPassengerType))
