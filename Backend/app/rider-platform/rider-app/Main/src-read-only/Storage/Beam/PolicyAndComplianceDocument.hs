{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PolicyAndComplianceDocument where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PolicyAndComplianceDocumentT f = PolicyAndComplianceDocumentT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    enabled :: (B.C f Kernel.Prelude.Bool),
    id :: (B.C f Kernel.Prelude.Text),
    isMandatory :: (B.C f Kernel.Prelude.Bool),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    metadata :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    policyType :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    url :: (B.C f Kernel.Prelude.Text),
    version :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PolicyAndComplianceDocumentT where
  data PrimaryKey PolicyAndComplianceDocumentT f = PolicyAndComplianceDocumentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PolicyAndComplianceDocumentId . id

type PolicyAndComplianceDocument = PolicyAndComplianceDocumentT Identity

$(enableKVPG (''PolicyAndComplianceDocumentT) [('id)] [[('policyType)]])

$(mkTableInstances (''PolicyAndComplianceDocumentT) "policy_and_compliance_document")
