{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PayoutInstanceCalculation where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PayoutInstanceCalculation = PayoutInstanceCalculation
  { endTime :: Kernel.Prelude.UTCTime,
    fromVendorId :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation,
    instanceBalance :: Kernel.Types.Common.HighPrecMoney,
    runningBalance :: Kernel.Types.Common.HighPrecMoney,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementMode :: Domain.Types.PayoutInstanceCalculation.SettlementMode,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.PayoutInstanceCalculation.PayoutStatus,
    toVendorId :: Data.Text.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PayoutStatus = Pending | Settled | SettleInNext deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SettlementMode = Manual | Online deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PayoutStatus))

$(mkHttpInstancesForEnum (''PayoutStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SettlementMode))

$(mkHttpInstancesForEnum (''SettlementMode))
