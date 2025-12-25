{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ParkingTransaction where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data ParkingTransaction = ParkingTransaction
  { amount :: Kernel.Types.Common.HighPrecMoney,
    endTime :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.ParkingTransaction.ParkingTransaction,
    parkingLotId :: Kernel.Prelude.Text,
    paymentOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.ParkingTransaction.StatusType,
    vehicleNumber :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data StatusType
  = Pending
  | Booked
  | Failed
  | RefundInitiated
  | RefundPending
  | Refunded
  | RefundFailed
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''StatusType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''StatusType)
