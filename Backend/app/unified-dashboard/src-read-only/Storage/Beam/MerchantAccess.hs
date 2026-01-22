{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantAccess where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Tools.Beam.UtilsTH

data MerchantAccessT f = MerchantAccessT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Data.Text.Text),
    is2faEnabled :: (B.C f Kernel.Prelude.Bool),
    merchantId :: (B.C f Data.Text.Text),
    merchantShortId :: (B.C f Data.Text.Text),
    operatingCity :: (B.C f Kernel.Types.Beckn.Context.City),
    personId :: (B.C f Data.Text.Text),
    secretKey :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantAccessT where
  data PrimaryKey MerchantAccessT f = MerchantAccessId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantAccessId . id

type MerchantAccess = MerchantAccessT Identity

$(enableKVPG (''MerchantAccessT) [('id)] [[('personId)]])

$(mkTableInstances (''MerchantAccessT) "merchant_access")
