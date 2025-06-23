{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.Error where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseBody, responseStatusCode))

data CMRLErrorResp = CMRLErrorResp
  { isError :: Bool,
    responseException :: Maybe ResponseException
  }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype ResponseException = ResponseException
  { exceptionMessage :: Maybe Text
  }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

data CMRLError
  = Unauthorized Text -- credentials missing or token has expired
  | Forbidden Text -- ip is not whitelisted from cmrl side
  | SomethingWentWrong Text -- all other error codes
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CMRLError

instance FromResponse CMRLError where
  fromResponse resp = do
    let mRespBody = decode $ responseBody resp
    let errorMessage =
          case mRespBody of
            Just (resp_ :: CMRLErrorResp) -> fromMaybe "" (resp_.responseException >>= (.exceptionMessage))
            Nothing -> "Error body decode failed"
    case statusCode (responseStatusCode resp) of
      401 -> Just $ Unauthorized errorMessage
      403 -> Just $ Forbidden errorMessage
      _ -> Just $ SomethingWentWrong errorMessage

instance IsBaseError CMRLError where
  toMessage = \case
    Unauthorized message -> Just $ "Unauthorized: " <> show message
    Forbidden message -> Just $ "Forbidden: " <> show message
    SomethingWentWrong message -> Just $ "SomethingWentWrong: " <> show message

instance IsHTTPError CMRLError where
  toErrorCode = \case
    Unauthorized _ -> "UNAUTHORIZED"
    Forbidden _ -> "FORBIDDEN"
    SomethingWentWrong _ -> "SOMETHING_WENT_WRONG"

  toHttpCode = \case
    Unauthorized _ -> E401
    Forbidden _ -> E403
    SomethingWentWrong _ -> E500

instance IsAPIError CMRLError
