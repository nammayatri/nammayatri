{-# LANGUAGE TemplateHaskell #-}

module Lib.Scheduler.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

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
