{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CancellationDuesDetails where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.CancellationDuesDetails
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data CancellationDuesDetailsT f = CancellationDuesDetailsT
  { cancellationAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    cancellationCollectionMode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    cancellationCommission :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cancellationCommissionTaxBreakup :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    cancellationConsequenceRowId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    cancellationFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cancellationFeeTax :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cancellationFeeTaxBreakup :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    id :: B.C f Kernel.Prelude.Text,
    overdueCancellationCharge :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    overdueCancellationCommission :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    overdueCancellationCommissionTaxBreakup :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    overdueCancellationTax :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    overdueCancellationTaxBreakup :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    paymentStatus :: B.C f Domain.Types.CancellationDuesDetails.CancellationDuesPaymentStatus,
    rideId :: B.C f Kernel.Prelude.Text,
    riderId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table CancellationDuesDetailsT where
  data PrimaryKey CancellationDuesDetailsT f = CancellationDuesDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CancellationDuesDetailsId . id

type CancellationDuesDetails = CancellationDuesDetailsT Identity

$(enableKVPG ''CancellationDuesDetailsT ['id] [['rideId]])

$(mkTableInstances ''CancellationDuesDetailsT "cancellation_dues_details")
