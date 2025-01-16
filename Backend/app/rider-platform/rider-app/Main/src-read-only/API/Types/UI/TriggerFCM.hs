{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TriggerFCM where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data FCMEntityData = FCMEntityData {channelId :: Kernel.Prelude.Maybe Data.Text.Text, personId :: Kernel.Types.Id.Id Domain.Types.Person.Person, source :: Kernel.Prelude.Maybe MessageSource}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MessageSource
  = USER
  | TRUSTED_CONTACT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TriggerFcmReq = TriggerFcmReq
  { body :: Data.Text.Text,
    channelId :: Kernel.Prelude.Maybe Data.Text.Text,
    chatPersonId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    showNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    source :: Kernel.Prelude.Maybe MessageSource,
    title :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
