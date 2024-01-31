{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSRecon where

import qualified Database.Beam as B
import qualified Domain.Types.FRFSRecon
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data FRFSReconT f = FRFSReconT
  { beneficiaryIFSC :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    buyerFinderFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    collectorIFSC :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    collectorSubscriberId :: B.C f Kernel.Prelude.Text,
    date :: B.C f Kernel.Prelude.Text,
    destinationStationCode :: B.C f Kernel.Prelude.Text,
    differenceAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fare :: B.C f Kernel.Types.Common.HighPrecMoney,
    frfsTicketBookingId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    message :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    networkOrderId :: B.C f Kernel.Prelude.Text,
    receiverSubscriberId :: B.C f Kernel.Prelude.Text,
    settlementAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    settlementDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    settlementReferenceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    sourceStationCode :: B.C f Kernel.Prelude.Text,
    ticketNumber :: B.C f Kernel.Prelude.Text,
    ticketQty :: B.C f Kernel.Prelude.Int,
    time :: B.C f Kernel.Prelude.Text,
    totalOrderValue :: B.C f Kernel.Types.Common.HighPrecMoney,
    transactionRefNumber :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSReconT where
  data PrimaryKey FRFSReconT f = FRFSReconId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = FRFSReconId . id

type FRFSRecon = FRFSReconT Identity

$(enableKVPG ''FRFSReconT ['id] [])

$(mkTableInstances ''FRFSReconT "frfs_recon")
