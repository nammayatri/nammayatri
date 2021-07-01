{-# LANGUAGE TemplateHaskell #-}

module Types.Error
  ( module Types.Error,
    module Beckn.Types.Error,
  )
where

import Beckn.Types.Error hiding (GatewayError)
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import EulerHS.Prelude

data GatewayError
  = NoProviders
  deriving (Eq, Show)

instanceExceptionWithParent 'BecknAPIException ''GatewayError

instance IsBaseError GatewayError

instance IsHTTPError GatewayError where
  toErrorCode NoProviders = "CORE001"

instance IsBecknAPIError GatewayError where
  toType NoProviders = DOMAIN_ERROR