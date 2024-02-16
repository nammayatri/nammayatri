{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketBookingPayment where

import qualified Database.Beam as B
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Tools.Beam.UtilsTH

data FRFSTicketBookingPaymentT f = FRFSTicketBookingPaymentT
  { frfsTicketBookingId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    paymentOrderId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketBookingPaymentT where
  data PrimaryKey FRFSTicketBookingPaymentT f = FRFSTicketBookingPaymentId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketBookingPaymentId . id

type FRFSTicketBookingPayment = FRFSTicketBookingPaymentT Identity

$(enableKVPG ''FRFSTicketBookingPaymentT ['id] [])

$(mkTableInstances ''FRFSTicketBookingPaymentT "frfs_ticket_booking_payment")
