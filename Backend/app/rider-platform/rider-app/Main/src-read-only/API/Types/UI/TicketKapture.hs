{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TicketKapture where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data TicketKaptureResp = TicketKaptureResp {encryptedCc :: Data.Text.Text, encryptedIv :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
