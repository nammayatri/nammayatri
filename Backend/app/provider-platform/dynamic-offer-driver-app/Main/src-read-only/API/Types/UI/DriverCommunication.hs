{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverCommunication where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Communication
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data DriverCommunicationItem = DriverCommunicationItem
  { body :: Data.Text.Text,
    communicationId :: Kernel.Types.Id.Id Domain.Types.Communication.Communication,
    isRead :: Kernel.Prelude.Bool,
    senderName :: Kernel.Prelude.Maybe Data.Text.Text,
    sentAt :: Kernel.Prelude.UTCTime,
    title :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverCommunicationListRes = DriverCommunicationListRes {communications :: [DriverCommunicationItem], hasMore :: Kernel.Prelude.Bool, unreadCount :: Kernel.Prelude.Int}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
