{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.SendWelcomeEmail
  ( sendWelcomeEmail,
    SendWelcomeEmailReq (..),
    renderTemplate,
  )
where

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Domain.Types.MerchantMessage as DMM
import qualified Email.Flow as Email
import qualified Email.Types as EmailT
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CQMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Tools.Fields as Fields

-- | Details of a just-created Control Centre user. @password@ is the plaintext
-- the creating admin set: never log this value.
data SendWelcomeEmailReq = SendWelcomeEmailReq
  { email :: Text,
    roleName :: Text,
    password :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

instance HideSecrets SendWelcomeEmailReq where
  hideSecrets req = req {password = ""}

defaultSubject :: Text
defaultSubject = "Welcome to Control Centre"

-- | Sends the Control Centre welcome email, using the merchant message template
-- @DASHBOARD_WELCOME_EMAIL@ of the merchant's operating city. The From address
-- and subject come from the template row (@senderEmail@, @title@); the support
-- address is the first entry of the operating city's @supportEmails@.
--
-- A merchant/city without a template, a sender, or a support address is skipped
-- (logged, not an error): such tenants simply have no welcome email.
sendWelcomeEmail ::
  Maybe Text ->
  Text ->
  Context.City ->
  SendWelcomeEmailReq ->
  Flow APISuccess
sendWelcomeEmail apiKey merchantShortIdText city req = do
  dashboardToken <- asks (.dashboardToken)
  unless (Just dashboardToken == apiKey) $
    throwError (AuthBlocked "Invalid BPP dashboard token")
  merchant <- findMerchantByShortId (ShortId merchantShortIdText)
  merchantOpCity <- CQMOC.getMerchantOpCity merchant (Just city)
  mbTemplate <- CQMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCity.id DMM.DASHBOARD_WELCOME_EMAIL Nothing Nothing
  let mbSupportEmail = case merchantOpCity.supportEmails of
        Just (supportEmail : _) -> Just supportEmail
        _ -> Nothing
  case mbTemplate of
    Nothing -> skip merchantOpCity.id.getId "no DASHBOARD_WELCOME_EMAIL template"
    Just template -> case (template.senderEmail, mbSupportEmail) of
      (Just fromEmail, Just supportEmail) -> do
        let subject = fromMaybe defaultSubject template.title
            body = renderTemplate req supportEmail template.message
        emailServiceConfig <- asks (.emailServiceConfig)
        result <- liftIO $ E.try @E.SomeException $ Email.sendPlainEmail emailServiceConfig fromEmail [req.email] subject body EmailT.Text
        case result of
          Left _ -> do
            logError $ "Failed to send dashboard welcome email for merchantOperatingCityId " <> merchantOpCity.id.getId
            throwError $ InternalError "Failed to send welcome email"
          Right _ -> pure Success
      _ -> skip merchantOpCity.id.getId "template has no senderEmail or city has no supportEmails"
  where
    skip opCityId reason = do
      logWarning $ "Skipping dashboard welcome email for merchantOperatingCityId " <> opCityId <> ": " <> reason
      pure Success

-- | Fills the template placeholders. The password is substituted last so that
-- placeholder-like text inside it is never expanded.
renderTemplate :: SendWelcomeEmailReq -> Text -> Text -> Text
renderTemplate req supportEmail =
  replacePlaceholder "password" req.password
    . replacePlaceholder "supportEmail" supportEmail
    . replacePlaceholder "email" req.email
    . replacePlaceholder "role" req.roleName
  where
    replacePlaceholder key = T.replace ("{#" <> key <> "#}")
