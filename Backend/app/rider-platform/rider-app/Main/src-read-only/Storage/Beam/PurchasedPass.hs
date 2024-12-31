{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PurchasedPass where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PurchasedPass
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PurchasedPassT f = PurchasedPassT
  { expiryDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    passId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    shortId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.PurchasedPass.StatusType),
    validTripsLeft :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PurchasedPassT where
  data PrimaryKey PurchasedPassT f = PurchasedPassId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PurchasedPassId . id

type PurchasedPass = PurchasedPassT Identity

$(enableKVPG (''PurchasedPassT) [('id)] [])

$(mkTableInstances (''PurchasedPassT) "purchased_pass")
