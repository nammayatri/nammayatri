{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetReferral where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetReferralT f = FleetReferralT
  { fleetOwnerId :: (B.C f Data.Text.Text),
    linkedAt :: (B.C f Kernel.Prelude.UTCTime),
    referralCode :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetReferralT where
  data PrimaryKey FleetReferralT f = FleetReferralId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetReferralId . referralCode

type FleetReferral = FleetReferralT Identity

$(enableKVPG (''FleetReferralT) [('referralCode)] [[('fleetOwnerId)]])

$(mkTableInstances (''FleetReferralT) "fleet_referral")
