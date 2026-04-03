{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.Merchant where
import EulerHS.Prelude hiding (id)
import Servant hiding (Summary)
import "lib-dashboard" Tools.Auth
import Data.OpenApi (ToSchema)
import qualified "lib-dashboard" Domain.Types.Person
import qualified Data.Text
import qualified Domain.Types.MerchantConfigs
import qualified Kernel.Prelude



data MerchantUserList
    = MerchantUserList {merchantUserList :: [Domain.Types.Person.PersonAPIEntity]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data ResetPasswordReq
    = ResetPasswordReq {email :: Data.Text.Text, newPassword :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data SetMerchantConfigReq
    = SetMerchantConfigReq {merchantShortId :: Data.Text.Text, webHookHeaders :: [Domain.Types.MerchantConfigs.WebHookHeaders], webhookUrl :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data WebHookConfigPreferenceReq
    = WebHookConfigPreferenceReq {preference :: Kernel.Prelude.Bool}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



