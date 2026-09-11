{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.Offer where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Offer

data OfferT f = OfferT
  { autoApply :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f Kernel.Types.Common.Currency,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    discountType :: B.C f Lib.Payment.Domain.Types.Offer.DiscountType,
    discountValue :: B.C f Kernel.Types.Common.HighPrecMoney,
    frequencyType :: B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Offer.OfferFrequency),
    id :: B.C f Kernel.Prelude.Text,
    isActive :: B.C f Kernel.Prelude.Bool,
    isHidden :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxApplyCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    maxDiscount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minimumAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    offerCode :: B.C f Kernel.Prelude.Text,
    offerEligibilityJsonLogic :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    offerType :: B.C f Lib.Payment.Domain.Types.Offer.OfferType,
    sponsoredBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    title :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    tnc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    validFrom :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    validTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table OfferT where
  data PrimaryKey OfferT f = OfferId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OfferId . id

type Offer = OfferT Identity

$(enableKVPG ''OfferT ['id] [['offerCode]])

$(mkTableInstancesGenericSchema ''OfferT "offer")
