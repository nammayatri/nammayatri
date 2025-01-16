{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantConfigs where

import qualified Database.Beam as B
import qualified Domain.Types.MerchantConfigs
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantConfigsT f = MerchantConfigsT
  { id :: B.C f Kernel.Prelude.Text,
    merchantShortId :: B.C f Kernel.Prelude.Text,
    requestWebHook :: B.C f Kernel.Prelude.Bool,
    webHookHeaders :: B.C f [Domain.Types.MerchantConfigs.WebHookHeaders],
    webHookUrl :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantConfigsT where
  data PrimaryKey MerchantConfigsT f = MerchantConfigsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantConfigsId . id

type MerchantConfigs = MerchantConfigsT Identity

$(enableKVPG ''MerchantConfigsT ['id] [])

$(mkTableInstances ''MerchantConfigsT "merchant_configs")
