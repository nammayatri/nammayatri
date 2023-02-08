{-# LANGUAGE TemplateHaskell #-}

module Tools.Error (module Tools.Error, module Reexport) where

import Kernel.Prelude
import Kernel.Types.Error as Reexport
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError

newtype ImplementationError
  = NotImplemented Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ImplementationError

instance IsBaseError ImplementationError where
  toMessage = \case
    NotImplemented msg -> Just msg

instance IsHTTPError ImplementationError where
  toErrorCode = \case
    NotImplemented _ -> "NOT_IMPLEMENTED"
  toHttpCode = \case
    NotImplemented _ -> E501

instance IsAPIError ImplementationError
