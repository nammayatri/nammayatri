{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ReferalPayout where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ReferalPayoutT f = ReferalPayoutT
  { id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table ReferalPayoutT where
  data PrimaryKey ReferalPayoutT f = ReferalPayoutId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReferalPayoutId . id

type ReferalPayout = ReferalPayoutT Identity

$(enableKVPG (''ReferalPayoutT) [('id)] [])

$(mkTableInstances (''ReferalPayoutT) "referal_payout")
