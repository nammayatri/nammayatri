{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MessageTranslation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Types
import qualified Data.Time
import qualified Database.Beam as B



data MessageTranslationT f
    = MessageTranslationT {createdAt :: (B.C f Data.Time.LocalTime),
                           description :: (B.C f Kernel.Prelude.Text),
                           label :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           language :: (B.C f Kernel.External.Types.Language),
                           messageId :: (B.C f Kernel.Prelude.Text),
                           shortDescription :: (B.C f Kernel.Prelude.Text),
                           title :: (B.C f Kernel.Prelude.Text)}
    deriving (Generic, B.Beamable)
instance B.Table MessageTranslationT
    where data PrimaryKey MessageTranslationT f = MessageTranslationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MessageTranslationId . messageId
type MessageTranslation = MessageTranslationT Identity

$(enableKVPG (''MessageTranslationT) [('messageId)] [])

$(mkTableInstances (''MessageTranslationT) "message_translation")

