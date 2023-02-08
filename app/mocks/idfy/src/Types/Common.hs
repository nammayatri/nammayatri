{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Common where

import Data.Aeson hiding (Error)
import Data.OpenApi
import EulerHS.Prelude hiding (state)
import Kernel.Types.Error.BaseError.HTTPError hiding (Error)
import Kernel.Types.Error.BaseError.HTTPError.FromResponse

type AccountId = Text

type ApiKey = Text

newtype Error = Error {message :: Text}
  deriving (Show, Generic)

instance IsAPIError Error

deriving newtype instance ToJSON Error

deriving newtype instance FromJSON Error

deriving newtype instance ToSchema Error

instance FromResponse Error where
  fromResponse = fromJsonResponse

instance IsBaseError Error where
  toMessage _ = Just "IDFY_VERIFICATION_UNAVAILABLE"

instance IsHTTPError Error where
  toErrorCode _ = "CORE002"
  toHttpCode _ = E404

instance IsBecknAPIError Error where
  toType _ = DOMAIN_ERROR -- only to satisfy current tests, FIXME maybe

instanceExceptionWithParent 'HTTPException ''Error
