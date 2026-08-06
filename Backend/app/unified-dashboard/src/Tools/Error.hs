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
    MerchantError (..),
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

data MerchantError
  = MerchantAlreadyExist Text
  | MerchantAccountLimitExceeded Text
  | UserDisabled
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MerchantError

instance IsBaseError MerchantError where
  toMessage = \case
    MerchantAlreadyExist shortId -> Just $ "Merchant with shortId \"" <> show shortId <> "\" already exist."
    MerchantAccountLimitExceeded shortId -> Just $ "Merchant with shortId \"" <> show shortId <> "\" already exist."
    UserDisabled -> Just "User is disabled. Contact admin"

instance IsHTTPError MerchantError where
  toErrorCode = \case
    MerchantAlreadyExist _ -> "MERCHANT_ALREADY_EXIST"
    MerchantAccountLimitExceeded _ -> "MERCHANT_ACCOUNT_LIMIT_EXCEEDED"
    UserDisabled -> "USER_DISABLED"
  toHttpCode = \case
    MerchantAlreadyExist _ -> E400
    MerchantAccountLimitExceeded _ -> E400
    UserDisabled -> E400

instance IsAPIError MerchantError
