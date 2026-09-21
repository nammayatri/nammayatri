{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverTag where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Servant
import Tools.Auth

data DriverTagAction
  = ADD_TAG
  | REMOVE_TAG
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverTagRes = DriverTagRes {driverTags :: [Lib.Yudhishthira.Types.TagNameValueExpiry]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverTagUpdateReq = DriverTagUpdateReq {action :: DriverTagAction, tagName :: Kernel.Prelude.Text, tagValue :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
