{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MultiModalNetwork where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MultiModalNetwork
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MultiModalNetworkT f = MultiModalNetworkT
  { id :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    networkClass :: (B.C f Kernel.Prelude.Text),
    networkCode :: (B.C f Kernel.Prelude.Text),
    networkType :: (B.C f Domain.Types.MultiModalNetwork.MultiModalNetworkType),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiModalNetworkT where
  data PrimaryKey MultiModalNetworkT f = MultiModalNetworkId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MultiModalNetworkId . id

type MultiModalNetwork = MultiModalNetworkT Identity

$(enableKVPG (''MultiModalNetworkT) [('id)] [])

$(mkTableInstances (''MultiModalNetworkT) "multi_modal_network")
