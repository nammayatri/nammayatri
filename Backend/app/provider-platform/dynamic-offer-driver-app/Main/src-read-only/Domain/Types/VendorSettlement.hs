{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VendorSettlement where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data VendorSettlement = VendorSettlement
  { createdAt :: Kernel.Prelude.UTCTime,
    fromVendorId :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VendorSettlement.VendorSettlement,
    runningBalance :: Kernel.Types.Common.HighPrecMoney,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementMode :: Domain.Types.VendorSettlement.VendorSettlementMode,
    status :: Domain.Types.VendorSettlement.VendorSettlementStatus,
    toVendorId :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data VendorSettlementMode = Manual | Online deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data VendorSettlementStatus = Pending | Settled | SettleInNext deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''VendorSettlementMode))

$(mkHttpInstancesForEnum (''VendorSettlementMode))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''VendorSettlementStatus))

$(mkHttpInstancesForEnum (''VendorSettlementStatus))
