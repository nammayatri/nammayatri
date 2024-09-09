{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MultiModalStops where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MultiModalStopsT f = MultiModalStopsT
  { id :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiModalStopsT where
  data PrimaryKey MultiModalStopsT f = MultiModalStopsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MultiModalStopsId . id

type MultiModalStops = MultiModalStopsT Identity

$(enableKVPG (''MultiModalStopsT) [('id)] [])

$(mkTableInstances (''MultiModalStopsT) "multi_modal_stops")
