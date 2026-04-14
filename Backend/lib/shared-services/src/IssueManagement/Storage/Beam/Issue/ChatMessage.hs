module IssueManagement.Storage.Beam.Issue.ChatMessage where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import GHC.Generics (Generic)
import qualified IssueManagement.Domain.Types.Issue.ChatMessage as Domain
import IssueManagement.Tools.UtilsTH hiding (Generic)

data ChatMessageT f = ChatMessageT
  { id :: B.C f Text,
    issueReportId :: B.C f Text,
    senderId :: B.C f Text,
    senderType :: B.C f Domain.ChatSenderType,
    chatContentType :: B.C f Domain.ChatContentType,
    message :: B.C f Text,
    mediaFileIds :: B.C f [Text],
    readAt :: B.C f (Maybe LocalTime),
    createdAt :: B.C f LocalTime,
    merchantId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table ChatMessageT where
  data PrimaryKey ChatMessageT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type ChatMessage = ChatMessageT Identity

$(enableKVPG ''ChatMessageT ['id] [['issueReportId]])

$(mkTableInstancesGenericSchema ''ChatMessageT "chat_message")
