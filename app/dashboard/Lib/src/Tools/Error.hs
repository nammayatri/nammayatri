{-# LANGUAGE TemplateHaskell #-}

module Tools.Error where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Utils.Common hiding (Error)

data Error = Error
  { code :: Text,
    message :: Text,
    details :: Value
  }
  deriving (Show, Generic, ToJSON, FromJSON, IsAPIError)

instance FromResponse Error where
  fromResponse = fromJsonResponse

instance IsHTTPError Error where
  toErrorCode Error {code} = code
  toHttpCode _ = E500

instance IsBaseError Error where
  toMessage Error {message} = Just message

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
