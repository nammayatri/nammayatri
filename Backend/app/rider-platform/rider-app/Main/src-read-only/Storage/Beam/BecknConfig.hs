{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BecknConfig where

import qualified Database.Beam as B
import qualified Domain.Types.BecknConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import Tools.Beam.UtilsTH

data BecknConfigT f = BecknConfigT
  { domain :: B.C f Kernel.Prelude.Text,
    gatewayUrl :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    registryUrl :: B.C f Kernel.Prelude.Text,
    subscriberId :: B.C f Kernel.Prelude.Text,
    subscriberUrl :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BecknConfigT where
  data PrimaryKey BecknConfigT f = BecknConfigId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = BecknConfigId . id

type BecknConfig = BecknConfigT Identity

$(enableKVPG ''BecknConfigT ['id] [])

$(mkTableInstances ''BecknConfigT "beckn_config")
