{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Error
  ( module Tools.Error,
    module Error,
  )
where

import Data.Aeson (decode)
import Kernel.Prelude
import Kernel.Types.Error as Error hiding (MerchantError)
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Kernel.Types.Error.BaseError.HTTPError.HttpCode
import Kernel.Utils.Common hiding (Error)
import Network.HTTP.Types.Status
import Servant.Client

data Error = Error
  { statusCode :: HttpCode,
    contents :: ErrorResponseContents
  }
  deriving (Show, Generic, IsAPIError)

data ErrorResponseContents = ErrorResponseContents
  { errorCode :: Text,
    errorMessage :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, IsAPIError)

instance FromResponse Error where
  fromResponse (Response (Status code _) _ _ body) = Error (codeToHttpCodeWith500Default code) <$> decode body

instance IsHTTPError Error where
  toErrorCode err = err.contents.errorCode
  toHttpCode err = err.statusCode

instance IsBaseError Error where
  toMessage err = err.contents.errorMessage

instance IsBecknAPIError Error where
  toType _ = DOMAIN_ERROR

instanceExceptionWithParent 'HTTPException ''Error

data SuspectFlaggedError
  = SuspectAlreadyFlaggedByMerchant Text
  | BulkUploadLimitExceeded Int
  | BulkSearchLimitExceeded Int
  | FlagRequestNotFound
  | FlagRequestAlreadyProcessed
  | InvalidAdminApproval
  | InvalidSearchSuspectRequest
  | SuspectNotFound
  | AuthTokenNotFound
  | RequestAlreadyExists
  | RequestAlreadyProcessed
  | FlagCategoryAlreadyExists
  | NotificationNotFound
  | SuspectDlOrVoterIdRequired
  | MerchantConfigNotFound
  | InvalidWebhookUrl
  | FlaggedByDecodeError
  | MerchantDisabled
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SuspectFlaggedError

instance IsBaseError SuspectFlaggedError where
  toMessage = \case
    SuspectAlreadyFlaggedByMerchant merchantId -> Just $ "Suspect is already flagged by merchant with merchanId \"" <> show merchantId <> "."
    BulkUploadLimitExceeded limit -> Just $ "Bulk  upload limit exceeded, the limit is " <> show limit <> "."
    BulkSearchLimitExceeded limit -> Just $ "Bulk search limit exceeded, the limit is " <> show limit <> "."
    FlagRequestNotFound -> Just "Flag request not found."
    FlagRequestAlreadyProcessed -> Just "Flag request already processed."
    InvalidAdminApproval -> Just "Invalid admin approval."
    InvalidSearchSuspectRequest -> Just "Invalid search suspect request.Expected either dl or voterId."
    SuspectNotFound -> Just "Suspect not found with dl or voterId ."
    AuthTokenNotFound -> Just "Auth token not found."
    RequestAlreadyExists -> Just "Request already exists."
    RequestAlreadyProcessed -> Just "Request already processed."
    FlagCategoryAlreadyExists -> Just "Flag category already exists."
    NotificationNotFound -> Just "Notification not found."
    SuspectDlOrVoterIdRequired -> Just "Either dl or voterId is required."
    MerchantConfigNotFound -> Just "Merchant config not found."
    InvalidWebhookUrl -> Just "Invalid webhook url."
    FlaggedByDecodeError -> Just "Error decoding flaggedBy."
    MerchantDisabled -> Just "Merchant is disabled."

instance IsHTTPError SuspectFlaggedError where
  toErrorCode = \case
    SuspectAlreadyFlaggedByMerchant _ -> "Suspect_ALREADY_FLAGGED_BY_MERCHANT"
    BulkUploadLimitExceeded _ -> "BULK_UPLOAD_LIMIT_EXCEEDED"
    BulkSearchLimitExceeded _ -> "BULK_SEARCH_LIMIT_EXCEEDED"
    FlagRequestNotFound -> "FLAG_REQUEST_NOT_FOUND"
    FlagRequestAlreadyProcessed -> "FLAG_REQUEST_ALREADY_PROCESSED"
    InvalidAdminApproval -> "INVALID_ADMIN_APPROVAL"
    InvalidSearchSuspectRequest -> "INVALID_SEARCH_SUSPECT_REQUEST"
    SuspectNotFound -> "SUSPECT_NOT_FOUND"
    AuthTokenNotFound -> "AUTH_TOKEN_NOT_FOUND"
    RequestAlreadyExists -> "REQUEST_ALREADY_EXISTS"
    RequestAlreadyProcessed -> "REQUEST_ALREADY_PROCESSED"
    FlagCategoryAlreadyExists -> "FLAG_CATEGORY_ALREADY_EXISTS"
    NotificationNotFound -> "NOTIFICATION_NOT_FOUND"
    SuspectDlOrVoterIdRequired -> "SUSPECT_DL_OR_VOTER_ID_REQUIRED"
    MerchantConfigNotFound -> "MERCHANT_CONFIG_NOT_FOUND"
    InvalidWebhookUrl -> "INVALID_WEBHOOK_URL"
    FlaggedByDecodeError -> "FLAGGED_BY_DECODE_ERROR"
    MerchantDisabled -> "MERCHANT_DISABLED"

  toHttpCode = \case
    SuspectAlreadyFlaggedByMerchant _ -> E400
    BulkUploadLimitExceeded _ -> E400
    BulkSearchLimitExceeded _ -> E400
    FlagRequestNotFound -> E404
    FlagRequestAlreadyProcessed -> E400
    InvalidAdminApproval -> E400
    InvalidSearchSuspectRequest -> E400
    SuspectNotFound -> E404
    AuthTokenNotFound -> E404
    RequestAlreadyExists -> E400
    RequestAlreadyProcessed -> E400
    FlagCategoryAlreadyExists -> E400
    NotificationNotFound -> E404
    SuspectDlOrVoterIdRequired -> E400
    MerchantConfigNotFound -> E404
    InvalidWebhookUrl -> E400
    FlaggedByDecodeError -> E400
    MerchantDisabled -> E400

instance IsAPIError SuspectFlaggedError

data SafetyWebhookError
  = WebhookNotDeliver
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SafetyWebhookError

instance FromResponse SafetyWebhookError where
  fromResponse resp = case Network.HTTP.Types.Status.statusCode $ responseStatusCode resp of
    _ -> Just WebhookNotDeliver

instance IsBaseError SafetyWebhookError where
  toMessage = \case
    WebhookNotDeliver -> Just "Something in your header or request body was malformed."

instance IsHTTPError SafetyWebhookError where
  toErrorCode = \case
    WebhookNotDeliver -> "WEBHOOK_NOT_DELIVER"

instance IsAPIError SafetyWebhookError
