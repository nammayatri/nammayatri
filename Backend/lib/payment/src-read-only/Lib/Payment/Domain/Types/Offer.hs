{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.Offer where

import qualified Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Offer = Offer
  { autoApply :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    discountType :: Lib.Payment.Domain.Types.Offer.DiscountType,
    discountValue :: Kernel.Types.Common.HighPrecMoney,
    frequencyType :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Offer.OfferFrequency,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer,
    isActive :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxApplyCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    minimumAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    offerCode :: Kernel.Prelude.Text,
    offerEligibilityJsonLogic :: Kernel.Prelude.Maybe Data.Aeson.Value,
    offerType :: Lib.Payment.Domain.Types.Offer.OfferType,
    sponsoredBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tnc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data DiscountType = FLAT | PERCENTAGE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data OfferFrequency = HOURLY | DAILY | WEEKLY | MONTHLY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data OfferType = DISCOUNT | CASHBACK deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DiscountType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''OfferFrequency)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''OfferType)
