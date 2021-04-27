{-# LANGUAGE TemplateHaskell #-}

module Types.Error
  ( module Types.Error,
    module Beckn.Types.Error,
  )
where

import Beckn.Types.Error hiding (GatewayError)
import Beckn.Types.Error.BecknAPIError
import EulerHS.Prelude

data GatewayError
  = NoProviders
  deriving (Eq, Show)

instanceExceptionWithParent 'BecknAPIException ''GatewayError

instance IsAPIError GatewayError where
  toErrorCode NoProviders = "NO_PROVIDERS"

instance IsBecknAPIError GatewayError where
  toType NoProviders = DOMAIN_ERROR
  toBecknCode NoProviders = "CORE001"
