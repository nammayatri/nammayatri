{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PurchasedPassPayment where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PurchasedPass
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data PurchasedPassPayment = PurchasedPassPayment
  { endDate :: Data.Time.Calendar.Day,
    id :: Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment,
    orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    purchasedPassId :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    startDate :: Data.Time.Calendar.Day,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
