{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PaymentOrderOffer where

import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data PaymentOrderOffer = PaymentOrderOffer
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    offer_code :: Kernel.Prelude.Text,
    offer_id :: Kernel.Prelude.Text,
    paymentOrderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    responseJSON :: Kernel.Prelude.Text,
    status :: Kernel.External.Payment.Interface.Types.OfferState,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
