{-# LANGUAGE TemplateHaskell #-}

module Beckn.Scheduler.Error where

import Beckn.Prelude
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Type.Reflection

data JobDecodeError
  = InvalidJobType Text
  | InvalidJobData Text
  deriving (Show, IsBecknAPIError, Typeable)

instanceExceptionWithParent 'HTTPException ''JobDecodeError

instance IsBaseError JobDecodeError where
  toMessage = \case
    InvalidJobType invalidType -> Just $ "Invalid job type: " <> invalidType
    InvalidJobData invalidData -> Just $ "Invalid job data: " <> invalidData

instance IsHTTPError JobDecodeError where
  toErrorCode = \case
    InvalidJobType _ -> "INVALID_JOB_TYPE"
    InvalidJobData _ -> "INVALID_JOB_DATA"
  toHttpCode _ = E500

instance IsAPIError JobDecodeError
