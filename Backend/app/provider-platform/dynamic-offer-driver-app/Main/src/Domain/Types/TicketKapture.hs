{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketKapture where

import Data.Aeson (FromJSON, ToJSON, object, (.=))
import qualified Data.Aeson as A
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import EulerHS.Prelude (Generic, Show)
import qualified Kernel.External.Ticket.Kapture.Types as Kapture

data TaggedChatMessageContent
  = TextMessage Text
  | FileAttachments [Kapture.FileAttachment]
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TaggedChatMessage = TaggedChatMessage
  { chatMessage :: TaggedChatMessageContent,
    senderName :: Text,
    receiverName :: Text,
    sentDate :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
