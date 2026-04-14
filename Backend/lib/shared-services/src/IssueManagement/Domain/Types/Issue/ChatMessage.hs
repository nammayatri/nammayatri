{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Domain.Types.Issue.ChatMessage where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import EulerHS.Prelude hiding (id)
import IssueManagement.Common as Reexport
import qualified IssueManagement.Domain.Types.Issue.IssueReport as D
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Types.Id

data ChatSenderType
  = SENDER_RIDER
  | SENDER_DRIVER
  | SENDER_OPERATOR
  deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''ChatSenderType)

data ChatContentType
  = CHAT_TEXT
  | CHAT_MEDIA
  deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''ChatContentType)

data ChatMessage = ChatMessage
  { id :: Id ChatMessage,
    issueReportId :: Id D.IssueReport,
    senderId :: Id Person,
    senderType :: ChatSenderType,
    chatContentType :: ChatContentType,
    message :: Text,
    mediaFileIds :: [Id DMF.MediaFile],
    readAt :: Maybe UTCTime,
    createdAt :: UTCTime,
    merchantId :: Maybe (Id Merchant)
  }
  deriving (Generic, Show, Eq)
