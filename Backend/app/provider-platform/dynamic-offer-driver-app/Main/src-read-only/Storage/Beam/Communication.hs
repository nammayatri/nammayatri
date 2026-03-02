{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Communication where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Communication
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CommunicationT f = CommunicationT
  { body :: B.C f Kernel.Prelude.Text,
    channels :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    contentType :: B.C f Domain.Types.Communication.CommunicationContentType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    ctaButton :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    domain :: B.C f Domain.Types.Communication.CommunicationDomain,
    htmlBody :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    mediaUrls :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    scheduledAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    senderDisplayName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    senderId :: B.C f Kernel.Prelude.Text,
    senderRole :: B.C f Domain.Types.Communication.CommunicationSenderRole,
    status :: B.C f Domain.Types.Communication.CommunicationStatus,
    templateId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    templateName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    title :: B.C f Kernel.Prelude.Text,
    triggerType :: B.C f Domain.Types.Communication.CommunicationTriggerType,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    variables :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)
  }
  deriving (Generic, B.Beamable)

instance B.Table CommunicationT where
  data PrimaryKey CommunicationT f = CommunicationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CommunicationId . id

type Communication = CommunicationT Identity

$(enableKVPG ''CommunicationT ['id] [])

$(mkTableInstances ''CommunicationT "communication")
