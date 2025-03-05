{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.OperatorReferral where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OperatorReferralT f = OperatorReferralT
  { linkedAt :: (B.C f Kernel.Prelude.UTCTime),
    operatorId :: (B.C f Data.Text.Text),
    referralCode :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table OperatorReferralT where
  data PrimaryKey OperatorReferralT f = OperatorReferralId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = OperatorReferralId . referralCode

type OperatorReferral = OperatorReferralT Identity

$(enableKVPG (''OperatorReferralT) [('referralCode)] [[('operatorId)]])

$(mkTableInstances (''OperatorReferralT) "operator_referral")
