module ExternalBPP.ExternalAPI.Bus.TNSTC.Error where

import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.Common

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

surfaceTnstcFault :: (MonadCatch m, Log m) => Text -> m a -> m a
surfaceTnstcFault context act =
  act `catch` \(TNSTCFault code msg) -> do
    logError $ "TNSTC fault [" <> context <> "] code=" <> code <> " surfaced to rider: " <> msg
    throwError (InvalidRequest msg)
