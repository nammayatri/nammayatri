{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverFee where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverFee
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DriverFeeT f = DriverFeeT
  { addedToFeeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    amountPaidByCoin :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    autopayPaymentStage :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverFee.AutopayPaymentStage),
    badDebtDeclarationDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    badDebtRecoveryDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    billNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    cancellationPenaltyAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    collectedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    collectedAtVendorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    collectedBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    driverConsideredInPayoutSettlementAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverId :: B.C f Kernel.Prelude.Text,
    endTime :: B.C f Kernel.Prelude.UTCTime,
    feeType :: B.C f Domain.Types.DriverFee.FeeType,
    feeWithoutDiscount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    govtCharges :: B.C f Kernel.Types.Common.Money,
    govtChargesAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    hasSibling :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    notificationRetryCount :: B.C f Kernel.Prelude.Int,
    numRides :: B.C f Kernel.Prelude.Int,
    offerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    overlaySent :: B.C f Kernel.Prelude.Bool,
    payBy :: B.C f Kernel.Prelude.UTCTime,
    planId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    planMode :: B.C f (Kernel.Prelude.Maybe Domain.Types.Plan.PaymentMode),
    planOfferTitle :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    cgst :: B.C f Kernel.Types.Common.HighPrecMoney,
    platformFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    sgst :: B.C f Kernel.Types.Common.HighPrecMoney,
    refundEntityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    refundedAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    refundedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    refundedBy :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverFee.RefundedBy),
    schedulerTryCount :: B.C f Kernel.Prelude.Int,
    serviceName :: B.C f (Kernel.Prelude.Maybe Domain.Types.Plan.ServiceNames),
    siblingFeeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialZoneAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    specialZoneRideCount :: B.C f Kernel.Prelude.Int,
    splitOfDriverFeeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    stageUpdatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    startTime :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.DriverFee.DriverFeeStatus,
    totalEarnings :: B.C f Kernel.Types.Common.Money,
    totalEarningsAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    validDays :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    vehicleNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverFeeT where
  data PrimaryKey DriverFeeT f = DriverFeeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverFeeId . id

type DriverFee = DriverFeeT Identity

$(enableKVPG ''DriverFeeT ['id] [['driverId]])

$(mkTableInstances ''DriverFeeT "driver_fee")
