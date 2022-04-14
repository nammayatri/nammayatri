{-# LANGUAGE TemplateHaskell #-}

module Beckn.Scheduler.Error where

import Beckn.Prelude
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Type.Reflection

newtype JobDecodeError = InvalidJobType Text
  deriving (Show, IsBecknAPIError, Typeable)

instanceExceptionWithParent 'HTTPException ''JobDecodeError

instance IsBaseError JobDecodeError where
  toMessage = \case
    InvalidJobType invalidType -> Just $ "Invalid job type: " <> invalidType

instance IsHTTPError JobDecodeError where
  toErrorCode = \case
    InvalidJobType _ -> "INVALID_JOB_TYPE"
  toHttpCode _ = E500

instance IsAPIError JobDecodeError
