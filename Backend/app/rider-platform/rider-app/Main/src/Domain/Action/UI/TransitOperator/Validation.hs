{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.UI.TransitOperator.Validation where

import Data.Aeson (Object, Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (getDbHash)
import Kernel.Utils.Common (throwError)
import SharedLogic.External.Nandi.Types (NandiTable (..))
import Tools.Error

preprocessUpsertBody :: NandiTable -> Value -> Flow Value
preprocessUpsertBody EmployeesInternal (Array _) =
  throwError $ InvalidRequest "POST /row only accepts a single object; use POST /rows for batch upserts"
preprocessUpsertBody EmployeesInternal (Object obj) = Object <$> validateAndHashRow 0 obj
preprocessUpsertBody EmployeesInternal _ =
  throwError $ InvalidRequest "Request body for employees_internal must be a JSON object"
preprocessUpsertBody _ body = pure body

preprocessUpsertBodyAtIdx :: NandiTable -> Int -> Value -> Flow Value
preprocessUpsertBodyAtIdx EmployeesInternal idx (Object obj) = Object <$> validateAndHashRow idx obj
preprocessUpsertBodyAtIdx EmployeesInternal idx _ =
  throwError $ InvalidRequest $ "Row " <> show idx <> ": each element must be a JSON object"
preprocessUpsertBodyAtIdx _ _ body = pure body

validateAndHashRow :: Int -> Object -> Flow Object
validateAndHashRow rowIdx obj = do
  hasEmail <- checkStringField rowIdx obj "email"
  hasMobile <- checkStringField rowIdx obj "mobile_no"
  hasPassword <- checkStringField rowIdx obj "password"
  unless (hasEmail || hasMobile) $
    throwError $ InvalidRequest $ "Row " <> show rowIdx <> ": at least one of 'email' or 'mobile_no' must be present"
  when (hasEmail && not hasPassword) $
    throwError $ InvalidRequest $ "Row " <> show rowIdx <> ": 'password' is required when 'email' is provided"
  hashCredentials obj

-- Returns True if the field is a String, False if absent or Null, throws if present but wrong type.
checkStringField :: Int -> Object -> Text -> Flow Bool
checkStringField rowIdx obj field =
  case KeyMap.lookup (Key.fromText field) obj of
    Just (String _) -> pure True
    Just Null -> pure False
    Nothing -> pure False
    Just _ -> throwError $ InvalidRequest $ "Row " <> show rowIdx <> ": '" <> field <> "' must be a string"

hashCredentials :: Object -> Flow Object
hashCredentials obj = do
  obj' <- case KeyMap.lookup (Key.fromText "email") obj of
    Just (String email) -> do
      emailHash <- getDbHash email
      pure $ KeyMap.insert (Key.fromText "email_hash") (toJSON emailHash) $ KeyMap.delete (Key.fromText "email") obj
    _ -> pure obj
  case KeyMap.lookup (Key.fromText "password") obj' of
    Just (String password) -> do
      passwordHash <- getDbHash password
      pure $ KeyMap.insert (Key.fromText "password_hash") (toJSON passwordHash) $ KeyMap.delete (Key.fromText "password") obj'
    _ -> pure obj'
