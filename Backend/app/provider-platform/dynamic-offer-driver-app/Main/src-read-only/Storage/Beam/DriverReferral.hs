{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverReferral where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverReferralT f = DriverReferralT
  { driverId :: B.C f Data.Text.Text,
    linkedAt :: B.C f Kernel.Prelude.UTCTime,
    referralCode :: B.C f Data.Text.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverReferralT where
  data PrimaryKey DriverReferralT f = DriverReferralId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverReferralId . referralCode

type DriverReferral = DriverReferralT Identity

$(enableKVPG ''DriverReferralT ['referralCode] [['driverId]])

$(mkTableInstances ''DriverReferralT "driver_referral")
