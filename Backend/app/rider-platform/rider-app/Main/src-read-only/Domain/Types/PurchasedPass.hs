{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PurchasedPass where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Pass
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data PurchasedPass = PurchasedPass
  { id :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    orderShortId :: Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    passId :: Kernel.Types.Id.Id Domain.Types.Pass.Pass,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.PurchasedPass.PurchasedPass,
    status :: Domain.Types.PurchasedPass.StatusType,
    usedCount :: Kernel.Prelude.Int,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data StatusType = Pending | Active | Failed | Expired | RefundPending | RefundInitiated | Refunded deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
