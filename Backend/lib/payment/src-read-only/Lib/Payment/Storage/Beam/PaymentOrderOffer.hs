{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Payment.Storage.Beam.PaymentOrderOffer where
import Kernel.Prelude
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.External.Payment.Interface.Types
import qualified Database.Beam as B



data PaymentOrderOfferT f
    = PaymentOrderOfferT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          discountAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                          id :: (B.C f Kernel.Prelude.Text),
                          merchantId :: (B.C f Kernel.Prelude.Text),
                          merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                          offer_code :: (B.C f Kernel.Prelude.Text),
                          offer_id :: (B.C f Kernel.Prelude.Text),
                          paymentOrderId :: (B.C f Kernel.Prelude.Text),
                          payoutAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                          responseJSON :: (B.C f Kernel.Prelude.Text),
                          status :: (B.C f Kernel.External.Payment.Interface.Types.OfferState),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PaymentOrderOfferT
    where data PrimaryKey PaymentOrderOfferT f = PaymentOrderOfferId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PaymentOrderOfferId . id
type PaymentOrderOffer = PaymentOrderOfferT Identity

$(enableKVPG (''PaymentOrderOfferT) [('id)] [[('paymentOrderId)]])

$(mkTableInstancesGenericSchema (''PaymentOrderOfferT) "payment_order_offer")

