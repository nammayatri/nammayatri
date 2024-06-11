{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Merchant where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.MerchantConfigs
import qualified "lib-dashboard" Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import "lib-dashboard" Tools.Auth

data MerchantUserList = MerchantUserList {merchantUserList :: [Domain.Types.Person.PersonAPIEntity]} deriving (Generic, ToJSON, FromJSON, ToSchema)

data ResetPasswordReq = ResetPasswordReq {email :: Data.Text.Text, newPassword :: Data.Text.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data SetMerchantConfigReq = SetMerchantConfigReq {merchantShortId :: Data.Text.Text, webHookHeaders :: [Domain.Types.MerchantConfigs.WebHookHeaders], webhookUrl :: Data.Text.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data WebHookConfigPreferenceReq = WebHookConfigPreferenceReq {preference :: Kernel.Prelude.Bool} deriving (Generic, ToJSON, FromJSON, ToSchema)

{-
	DSL Source Link: file://./../../../../spec/API/merchant.yaml
-}
