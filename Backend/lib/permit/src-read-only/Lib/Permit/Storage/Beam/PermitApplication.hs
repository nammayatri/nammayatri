{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Beam.PermitApplication where

import qualified Data.Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Lib.Permit.Types

data PermitApplicationT f = PermitApplicationT
  { applicantId :: (B.C f Kernel.Prelude.Text),
    approvalLevel :: (B.C f Lib.Permit.Domain.Types.PermitApplication.ApprovalLevel),
    consentAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    decidedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    decidedById :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    decidedByName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    decisionReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    detailsEncrypted :: (B.C f Kernel.Prelude.Text),
    detailsHash :: (B.C f Kernel.External.Encryption.DbHash),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    permitDate :: (B.C f Data.Time.Day),
    permitType :: (B.C f Lib.Permit.Types.PermitType),
    primaryConflictHash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    qrTokenEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    qrTokenHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    referenceNo :: (B.C f Kernel.Prelude.Text),
    secondaryConflictHash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Lib.Permit.Domain.Types.PermitApplication.PermitStatus),
    submittedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validFrom :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PermitApplicationT where
  data PrimaryKey PermitApplicationT f = PermitApplicationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PermitApplicationId . id

type PermitApplication = PermitApplicationT Identity

$(enableKVPG (''PermitApplicationT) [('id)] [[('applicantId)], [('referenceNo)]])

$(mkTableInstancesGenericSchema (''PermitApplicationT) "permit_application")
