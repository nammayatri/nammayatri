{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Beam.PermitApplicant where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data PermitApplicantT f = PermitApplicantT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    mobileCountryCode :: (B.C f Kernel.Prelude.Text),
    mobileNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    mobileNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PermitApplicantT where
  data PrimaryKey PermitApplicantT f = PermitApplicantId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PermitApplicantId . id

type PermitApplicant = PermitApplicantT Identity

$(enableKVPG (''PermitApplicantT) [('id)] [])

$(mkTableInstancesGenericSchema (''PermitApplicantT) "permit_applicant")
