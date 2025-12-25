module ExternalBPP.ExternalAPI.Subway.CRIS.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse

newtype CRISError = CRISError
  { errorMessage :: Text
  }
  deriving (Show, Generic, IsBecknAPIError)
  deriving anyclass (Exception)

newtype CRISErrorUnhandled = CRISErrorUnhandled
  { errorMessage :: Text
  }
  deriving (Show, Generic, IsBecknAPIError)
  deriving anyclass (Exception)

instance FromResponse CRISError where
  fromResponse = Just . CRISError . show

instance FromResponse CRISErrorUnhandled where
  fromResponse = Just . CRISErrorUnhandled . show

instance IsBaseError CRISError where
  toMessage = Just . (.errorMessage)

instance IsBaseError CRISErrorUnhandled where
  toMessage = Just . (.errorMessage)

instance IsHTTPError CRISError where
  toErrorCode _ = "CRIS_ERROR"
  toHttpCode _ = E500

instance IsHTTPError CRISErrorUnhandled where
  toErrorCode _ = "CRIS_ERROR_UNHANDLED"
  toHttpCode _ = E500

instance IsAPIError CRISError

instance IsAPIError CRISErrorUnhandled
