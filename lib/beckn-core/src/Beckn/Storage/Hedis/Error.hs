{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Storage.Hedis.Error where

import Beckn.Prelude
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Type.Reflection

data HedisError
  = HedisReplyError String
  | HedisDecodeError Text
  | HedisTransactionAborted
  deriving (Show, Typeable, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''HedisError

instance IsBaseError HedisError where
  toMessage = \case
    HedisReplyError err -> Just $ show err
    HedisDecodeError err -> Just err
    HedisTransactionAborted -> Nothing

instance IsHTTPError HedisError where
  toErrorCode = \case
    HedisReplyError _ -> "REDIS_REPLY_ERROR"
    HedisDecodeError _ -> "REDIS_DECODE_ERROR"
    HedisTransactionAborted -> "REDIS_TRANSACTION_ABORTED"
  toHttpCode _ = E500

instance IsAPIError HedisError
