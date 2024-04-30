{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverFee where

import qualified Database.Beam as B
import qualified Domain.Types.DriverFee
import qualified Domain.Types.Plan
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DriverFeeT f = DriverFeeT
  { amountPaidByCoin :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    autopayPaymentStage :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverFee.AutopayPaymentStage)),
    badDebtDeclarationDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    badDebtRecoveryDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    billNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    collectedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    collectedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    endTime :: (B.C f Kernel.Prelude.UTCTime),
    feeType :: (B.C f Domain.Types.DriverFee.FeeType),
    feeWithoutDiscount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    govtCharges :: (B.C f Kernel.Types.Common.Money),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    notificationRetryCount :: (B.C f Kernel.Prelude.Int),
    numRides :: (B.C f Kernel.Prelude.Int),
    offerId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    overlaySent :: (B.C f Kernel.Prelude.Bool),
    payBy :: (B.C f Kernel.Prelude.UTCTime),
    planId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    planMode :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Plan.PaymentMode)),
    planOfferTitle :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    cgst :: (B.C f Kernel.Types.Common.HighPrecMoney),
    platformFee :: (B.C f Kernel.Types.Common.HighPrecMoney),
    sgst :: (B.C f Kernel.Types.Common.HighPrecMoney),
    schedulerTryCount :: (B.C f Kernel.Prelude.Int),
    serviceName :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Plan.ServiceNames)),
    specialZoneAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    specialZoneRideCount :: (B.C f Kernel.Prelude.Int),
    stageUpdatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    startTime :: (B.C f Kernel.Prelude.UTCTime),
    status :: (B.C f Domain.Types.DriverFee.DriverFeeStatus),
    totalEarnings :: (B.C f Kernel.Types.Common.Money),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverFeeT where
  data PrimaryKey DriverFeeT f = DriverFeeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverFeeId . id

type DriverFee = DriverFeeT Identity

$(enableKVPG (''DriverFeeT) [('id)] [[('driverId)]])

$(mkTableInstances (''DriverFeeT) "driver_fee")
