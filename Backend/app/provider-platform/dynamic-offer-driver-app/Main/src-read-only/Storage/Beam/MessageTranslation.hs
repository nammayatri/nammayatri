{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MessageTranslation where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MessageTranslationT f = MessageTranslationT
  { createdAt :: B.C f Data.Time.LocalTime,
    description :: B.C f Kernel.Prelude.Text,
    label :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    language :: B.C f Kernel.External.Types.Language,
    messageId :: B.C f Kernel.Prelude.Text,
    shortDescription :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table MessageTranslationT where
  data PrimaryKey MessageTranslationT f = MessageTranslationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MessageTranslationId . messageId

type MessageTranslation = MessageTranslationT Identity

$(enableKVPG ''MessageTranslationT ['messageId] [])

$(mkTableInstances ''MessageTranslationT "message_translation")
