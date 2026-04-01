{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.OfflineOffer where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common

data OfflineOfferT f = OfflineOfferT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    discountAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    offerCode :: (B.C f Kernel.Prelude.Text),
    offerId :: (B.C f Kernel.Prelude.Text),
    payoutAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    referenceId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Kernel.External.Payment.Interface.Types.OfferState),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table OfflineOfferT where
  data PrimaryKey OfflineOfferT f = OfflineOfferId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OfflineOfferId . id

type OfflineOffer = OfflineOfferT Identity

$(enableKVPG (''OfflineOfferT) [('id)] [[('referenceId)]])

$(mkTableInstancesGenericSchema (''OfflineOfferT) "offline_offer")
