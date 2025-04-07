module ExternalBPP.ExternalAPI.Subway.CRIS.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse

newtype CRISError = CRISError
  { errorMessage :: Text
  }
  deriving (Show, Generic, IsBecknAPIError)
  deriving anyclass (Exception)

instance FromResponse CRISError where
  fromResponse = Just . CRISError . show

instance IsBaseError CRISError where
  toMessage = Just . (.errorMessage)

instance IsHTTPError CRISError where
  toErrorCode _ = "CRIS_ERROR"
  toHttpCode _ = E500

instance IsAPIError CRISError
