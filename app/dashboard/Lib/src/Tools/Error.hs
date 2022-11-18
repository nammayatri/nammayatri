{-# LANGUAGE TemplateHaskell #-}

module Tools.Error where

import Beckn.Prelude
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Types.Error.BaseError.HTTPError.HttpCode
import Beckn.Utils.Common hiding (Error)
import Data.Aeson (decode)
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

data RoleError
  = RoleNotFound Text
  | RoleDoesNotExist Text
  | RoleNameExists Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RoleError

instance IsBaseError RoleError where
  toMessage = \case
    RoleNotFound roleId -> Just $ "Role with roleId \"" <> show roleId <> "\" not found."
    RoleDoesNotExist roleId -> Just $ "No role matches passed data \"" <> show roleId <> "\" not exist."
    RoleNameExists name -> Just $ "Role with name \"" <> show name <> "\" already exists."

instance IsHTTPError RoleError where
  toErrorCode = \case
    RoleNotFound _ -> "ROLE_NOT_FOUND"
    RoleDoesNotExist _ -> "ROLE_DOES_NOT_EXIST"
    RoleNameExists _ -> "ROLE_NAME_ALREADY_EXISTS"
  toHttpCode = \case
    RoleNotFound _ -> E500
    RoleDoesNotExist _ -> E400
    RoleNameExists _ -> E400

instance IsAPIError RoleError
