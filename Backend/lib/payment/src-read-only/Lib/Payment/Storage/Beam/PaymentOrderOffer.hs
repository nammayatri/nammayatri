{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PaymentOrderOffer where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data PaymentOrderOfferT f = PaymentOrderOfferT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    offer_code :: (B.C f Kernel.Prelude.Text),
    offer_id :: (B.C f Kernel.Prelude.Text),
    paymentOrderId :: (B.C f Kernel.Prelude.Text),
    responseJSON :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentOrderOfferT where
  data PrimaryKey PaymentOrderOfferT f = PaymentOrderOfferId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PaymentOrderOfferId . id

type PaymentOrderOffer = PaymentOrderOfferT Identity

$(enableKVPG (''PaymentOrderOfferT) [('id)] [[('paymentOrderId)]])

$(mkTableInstancesGenericSchema (''PaymentOrderOfferT) "payment_order_offer")
