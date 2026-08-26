{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CancellationConsequenceMatrix where

import qualified Database.Beam as B
import qualified Domain.Types.CancellationConsequenceMatrix
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.Extra.CancellationConsequenceMatrix
import qualified Domain.Types.MerchantPaymentMethod
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.DriverCoins.Types
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH

data CancellationConsequenceMatrixT f = CancellationConsequenceMatrixT
  { active :: B.C f Kernel.Prelude.Bool,
    area :: B.C f (Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area),
    blacklistDriverForRiderSeconds :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    cancelledBy :: B.C f (Kernel.Prelude.Maybe Lib.DriverCoins.Types.CancellationType),
    collectionMode :: B.C f (Kernel.Prelude.Maybe Domain.Types.CancellationConsequenceMatrix.ConsequenceCollectionMode),
    countsTowardCustomerCancellationStats :: B.C f Kernel.Prelude.Bool,
    countsTowardDriverCancellationRate :: B.C f Kernel.Prelude.Bool,
    customerCommissionAndTax :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.CancellationConsequenceMatrix.CommissionAndTax),
    customerDeduction :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.CancellationConsequenceMatrix.ConsequenceDeduction),
    customerNotificationKey :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverDeduction :: B.C f (Kernel.Prelude.Maybe Domain.Types.Extra.CancellationConsequenceMatrix.ConsequenceDeduction),
    driverNotificationKey :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    exemptDashboardBookings :: B.C f Kernel.Prelude.Bool,
    faultRule :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    faultVerdict :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    maxWaiveOffsPerPeriod :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    paymentInstrument :: B.C f (Kernel.Prelude.Maybe Domain.Types.MerchantPaymentMethod.PaymentInstrument),
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    vehicleServiceTier :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType),
    waiveOffAllowed :: B.C f Kernel.Prelude.Bool,
    waiveOffPeriodDays :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CancellationConsequenceMatrixT where
  data PrimaryKey CancellationConsequenceMatrixT f = CancellationConsequenceMatrixId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CancellationConsequenceMatrixId . id

type CancellationConsequenceMatrix = CancellationConsequenceMatrixT Identity

$(enableKVPG ''CancellationConsequenceMatrixT ['id] [])

$(mkTableInstances ''CancellationConsequenceMatrixT "cancellation_consequence_matrix")
