{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.EmailTemplate where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.EmailTemplate
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data EmailTemplateT f = EmailTemplateT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.External.Types.Language,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    templateKey :: B.C f Domain.Types.EmailTemplate.EmailTemplateKey,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    value :: B.C f Data.Aeson.Value
  }
  deriving (Generic, B.Beamable)

instance B.Table EmailTemplateT where
  data PrimaryKey EmailTemplateT f = EmailTemplateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EmailTemplateId . id

type EmailTemplate = EmailTemplateT Identity

$(enableKVPG ''EmailTemplateT ['id] [])

$(mkTableInstances ''EmailTemplateT "email_template")
