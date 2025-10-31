{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VendorSettlement where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VendorSettlement
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data VendorSettlementT f = VendorSettlementT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fromVendorId :: (B.C f Data.Text.Text),
    id :: (B.C f Data.Text.Text),
    runningBalance :: (B.C f Kernel.Types.Common.HighPrecMoney),
    settlementDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    settlementMode :: (B.C f Domain.Types.VendorSettlement.VendorSettlementMode),
    status :: (B.C f Domain.Types.VendorSettlement.VendorSettlementStatus),
    toVendorId :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table VendorSettlementT where
  data PrimaryKey VendorSettlementT f = VendorSettlementId (B.C f Data.Text.Text) (B.C f Data.Text.Text) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = VendorSettlementId <$> fromVendorId <*> id <*> toVendorId

type VendorSettlement = VendorSettlementT Identity

$(enableKVPG (''VendorSettlementT) [('fromVendorId), ('id), ('toVendorId)] [])

$(mkTableInstances (''VendorSettlementT) "vendor_settlement")
