{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultiChat where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data MultiChatReq = MultiChatReq {body :: Data.Text.Text, chatPersonId :: Kernel.Types.Id.Id Domain.Types.Person.Person, name :: Data.Text.Text, title :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
