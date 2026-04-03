{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Payment.Domain.Types.Offer where
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data Offer
    = Offer {createdAt :: Kernel.Prelude.UTCTime,
             currency :: Kernel.Types.Common.Currency,
             description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
             discountType :: Lib.Payment.Domain.Types.Offer.DiscountType,
             discountValue :: Kernel.Types.Common.HighPrecMoney,
             id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer,
             isActive :: Kernel.Prelude.Bool,
             maxDiscount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
             merchantId :: Kernel.Prelude.Text,
             merchantOperatingCityId :: Kernel.Prelude.Text,
             offerCode :: Kernel.Prelude.Text,
             offerEligibilityJsonLogic :: Kernel.Prelude.Maybe Data.Aeson.Value,
             offerType :: Lib.Payment.Domain.Types.Offer.OfferType,
             sponsoredBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
             title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
             tnc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show))
data DiscountType = FLAT | PERCENTAGE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)
data OfferType = DISCOUNT | CASHBACK deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''DiscountType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''OfferType))

