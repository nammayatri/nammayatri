{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MessageDictionary where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.MessageDictionary
import qualified Kernel.Prelude
import qualified Database.Beam as B



data MessageDictionaryT f
    = MessageDictionaryT {id :: (B.C f Kernel.Prelude.Text),
                          merchantId :: (B.C f Kernel.Prelude.Text),
                          merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                          messageKey :: (B.C f Kernel.Prelude.Text),
                          messageType :: (B.C f Domain.Types.MessageDictionary.MessageType),
                          createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table MessageDictionaryT
    where data PrimaryKey MessageDictionaryT f = MessageDictionaryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MessageDictionaryId . id
type MessageDictionary = MessageDictionaryT Identity

$(enableKVPG (''MessageDictionaryT) [('id)] [[('messageKey)], [('messageType)]])

$(mkTableInstances (''MessageDictionaryT) "message_dictionary")

