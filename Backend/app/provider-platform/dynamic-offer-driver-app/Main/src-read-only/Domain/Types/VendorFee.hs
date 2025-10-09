{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VendorFee where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.DriverFee
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data VendorFee = VendorFee
  { amount :: Kernel.Types.Common.HighPrecMoney,
    driverFeeId :: Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee,
    splitMethod :: Kernel.Prelude.Maybe Domain.Types.VendorFee.VendorFeeSplitMethod,
    vendorFeeProcessedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vendorId :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VendorFeeSplitMethod = Payout | Normal deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''VendorFeeSplitMethod)

$(mkHttpInstancesForEnum ''VendorFeeSplitMethod)
