module ExternalBPP.ExternalAPI.Bus.TNSTC.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError.HTTPError

data TNSTCFault = TNSTCFault
  { faultCode :: Text,
    faultMessage :: Text
  }
  deriving (Show, Generic, IsBecknAPIError)
  deriving anyclass (Exception)

instance IsBaseError TNSTCFault where
  toMessage e = Just e.faultMessage

instance IsHTTPError TNSTCFault where
  toErrorCode _ = "TNSTC_FAULT"
  toHttpCode _ = E500

instance IsAPIError TNSTCFault
