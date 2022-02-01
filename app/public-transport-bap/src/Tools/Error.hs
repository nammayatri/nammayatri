{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Tools.Error
  ( module Tools.Error,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Error as Reexport
import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError

data PublicTransportError
  = PublicTransportNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''PublicTransportError

instance IsBaseError PublicTransportError

instance IsHTTPError PublicTransportError where
  toErrorCode PublicTransportNotFound = "PUBLIC_TRANSPORT_NOT_FOUND"
  toHttpCode PublicTransportNotFound = E500

instance IsAPIError PublicTransportError