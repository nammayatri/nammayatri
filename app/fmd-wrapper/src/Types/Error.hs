{-# LANGUAGE TemplateHaskell #-}

module Types.Error
  ( module Types.Error,
    module Beckn.Types.Error,
  )
where

import Beckn.Types.Error
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import EulerHS.Prelude

data ErrorCode
  = CORE001
  | CORE002
  | CORE003
  | CORE004
  | CORE005
  | CORE006
  | CORE007
  | CORE008
  | CORE009
  | CORE010
  | FMD001
  | FMD002
  | FMD003
  | FMD004
  | FMD005
  | FMD006
  | FMD007
  | FMD008
  | FMD009
  | FMD010
  | FMD011
  | FMD012
  | FMD013
  | FMD014
  | FMD015
  | FMD016
  deriving (Eq, Show)

instanceExceptionWithParent 'BecknAPIException ''ErrorCode

instance IsBaseError ErrorCode

instance IsHTTPError ErrorCode where
  toErrorCode = show
  toHttpCode _ = E400

instance IsBecknAPIError ErrorCode where
  toType _ = DOMAIN_ERROR

data ErrorCodeWithMessage = ErrorCodeWithMessage Text ErrorCode deriving (Show)

instance IsBaseError ErrorCodeWithMessage where
  toMessage (ErrorCodeWithMessage msg _) = Just msg

instance IsHTTPError ErrorCodeWithMessage where
  toErrorCode (ErrorCodeWithMessage _ e) = toErrorCode e
  toHttpCode (ErrorCodeWithMessage _ e) = toHttpCode e
  toCustomHeaders (ErrorCodeWithMessage _ e) = toCustomHeaders e

instance IsBecknAPIError ErrorCodeWithMessage where
  toType (ErrorCodeWithMessage _ e) = toType e
  toPath (ErrorCodeWithMessage _ e) = toPath e

instanceExceptionWithParent 'BecknAPIException ''ErrorCodeWithMessage
