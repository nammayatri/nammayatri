{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PurchasedPass where

import Data.Aeson
import qualified Domain.Types.FRFSVehicleServiceTier
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data PurchasedPass = PurchasedPass
  { applicableVehicleServiceTiers :: [Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier],
    benefitDescription :: Kernel.Prelude.Text,
    benefitType :: Kernel.Prelude.Maybe Domain.Types.PurchasedPass.BenefitType,
    benefitValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    maxValidDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidTrips :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    orderShortId :: Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
    passAmount :: Kernel.Types.Common.HighPrecMoney,
    passCode :: Kernel.Prelude.Text,
    passName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    passTypeId :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.PurchasedPass.PurchasedPass,
    status :: Domain.Types.PurchasedPass.StatusType,
    usedCount :: Kernel.Prelude.Int,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BenefitType = FullSaving | FixedSaving | PercentageSaving deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data StatusType = Pending | Active | Failed | Expired | RefundPending | RefundInitiated | Refunded deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))
