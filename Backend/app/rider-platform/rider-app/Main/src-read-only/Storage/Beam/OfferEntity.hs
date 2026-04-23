{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.OfferEntity where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.OfferEntity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data OfferEntityT f = OfferEntityT
  { amountSaved :: B.C f Kernel.Types.Common.HighPrecMoney,
    autoApply :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    discountAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    entityId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Domain.Types.OfferEntity.EntityType,
    id :: B.C f Kernel.Prelude.Text,
    isHidden :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    offerCode :: B.C f Kernel.Prelude.Text,
    offerDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    offerId :: B.C f Kernel.Prelude.Text,
    offerSponsoredBy :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    offerTitle :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    offerTnc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    postOfferAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OfferEntityT where
  data PrimaryKey OfferEntityT f = OfferEntityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OfferEntityId . id

type OfferEntity = OfferEntityT Identity

$(enableKVPG ''OfferEntityT ['id] [['entityId]])

$(mkTableInstances ''OfferEntityT "offer_entity")
