{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MetaWebhookConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MetaWebhookConfigT f = MetaWebhookConfigT
  { accessToken :: (B.C f Kernel.Prelude.Text),
    apiVersion :: (B.C f Kernel.Prelude.Text),
    appSecret :: (B.C f Kernel.Prelude.Text),
    baseUrl :: (B.C f Kernel.Prelude.Text),
    botConfig :: (B.C f Data.Aeson.Value),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    phoneNumberId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    verifyToken :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table MetaWebhookConfigT where
  data PrimaryKey MetaWebhookConfigT f = MetaWebhookConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MetaWebhookConfigId . id

type MetaWebhookConfig = MetaWebhookConfigT Identity

$(enableKVPG (''MetaWebhookConfigT) [('id)] [[('phoneNumberId)]])

$(mkTableInstances (''MetaWebhookConfigT) "meta_config")
