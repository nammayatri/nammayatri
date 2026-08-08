{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VasBannerConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VasBannerConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VasBannerConfigT f = VasBannerConfigT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    deepLink :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    imageUrl :: (B.C f Kernel.Prelude.Text),
    linkType :: (B.C f Domain.Types.VasBannerConfig.VasBannerLinkType),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    priority :: (B.C f Kernel.Prelude.Int),
    subtitle :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    title :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validFrom :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    validTo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    whatsappTemplateId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table VasBannerConfigT where
  data PrimaryKey VasBannerConfigT f = VasBannerConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VasBannerConfigId . id

type VasBannerConfig = VasBannerConfigT Identity

$(enableKVPG (''VasBannerConfigT) [('id)] [])

$(mkTableInstances (''VasBannerConfigT) "vas_banner_config")
