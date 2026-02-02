{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.AlertWebhook where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data AlertAnnotations = AlertAnnotations {description :: Kernel.Prelude.Maybe Kernel.Prelude.Text, summary :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AlertDetail = AlertDetail {annotations :: AlertAnnotations, endsAt :: Kernel.Prelude.UTCTime, labels :: AlertLabels, startsAt :: Kernel.Prelude.UTCTime, status :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AlertLabels = AlertLabels
  { alert :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    alertgroup :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    alertname :: Kernel.Prelude.Text,
    severity :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VmAlertWebhookReq = VmAlertWebhookReq {alerts :: [AlertDetail], externalURL :: Kernel.Prelude.Maybe Kernel.Prelude.Text, receiver :: Kernel.Prelude.Maybe Kernel.Prelude.Text, status :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
