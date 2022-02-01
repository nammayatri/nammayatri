{-# LANGUAGE TemplateHaskell #-}

module Beckn.Mock.Exceptions where

import Beckn.Prelude
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Type.Reflection

newtype OrderError = OrderNotFound Text
  deriving (Show, IsBecknAPIError, Typeable)

instanceExceptionWithParent 'HTTPException ''OrderError

instance IsBaseError OrderError where
  toMessage = \case
    OrderNotFound orderId -> Just $ "Order not found:" <> show orderId

instance IsHTTPError OrderError where
  toErrorCode = \case
    OrderNotFound _ -> "ORDER_NOT_FOUND"
  toHttpCode _ = E500

instance IsAPIError OrderError
