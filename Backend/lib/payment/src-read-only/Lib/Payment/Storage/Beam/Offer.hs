{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Payment.Storage.Beam.Offer where
import Kernel.Prelude
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Offer
import qualified Data.Aeson
import qualified Database.Beam as B



data OfferT f
    = OfferT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
              currency :: (B.C f Kernel.Types.Common.Currency),
              description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              discountType :: (B.C f Lib.Payment.Domain.Types.Offer.DiscountType),
              discountValue :: (B.C f Kernel.Types.Common.HighPrecMoney),
              id :: (B.C f Kernel.Prelude.Text),
              isActive :: (B.C f Kernel.Prelude.Bool),
              maxDiscount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
              merchantId :: (B.C f Kernel.Prelude.Text),
              merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
              offerCode :: (B.C f Kernel.Prelude.Text),
              offerEligibilityJsonLogic :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
              offerType :: (B.C f Lib.Payment.Domain.Types.Offer.OfferType),
              sponsoredBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              title :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              tnc :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table OfferT
    where data PrimaryKey OfferT f = OfferId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = OfferId . id
type Offer = OfferT Identity

$(enableKVPG (''OfferT) [('id)] [[('offerCode)]])

$(mkTableInstancesGenericSchema (''OfferT) "offer")

