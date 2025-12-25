{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Message where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Message
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MessageT f = MessageT
  { messageType :: B.C f Domain.Types.Message.MessageType,
    alwaysTriggerOnOnboarding :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    createdAt :: B.C f Data.Time.LocalTime,
    description :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    label :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    likeCount :: B.C f Kernel.Prelude.Int,
    mediaFiles :: B.C f [Kernel.Prelude.Text],
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    shareable :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    shortDescription :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text,
    viewCount :: B.C f Kernel.Prelude.Int
  }
  deriving (Generic, B.Beamable)

instance B.Table MessageT where
  data PrimaryKey MessageT f = MessageId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MessageId . id

type Message = MessageT Identity

$(enableKVPG ''MessageT ['id] [])

$(mkTableInstancesWithTModifier ''MessageT "message" [("messageType", "type")])
