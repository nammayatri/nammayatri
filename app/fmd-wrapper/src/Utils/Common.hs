module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common as CoreCommon
import EulerHS.Prelude
import Types.Error

getClientConfig :: (MonadThrow m, Log m, FromJSON a) => Organization -> m a
getClientConfig org =
  let mconfig = org.info >>= decodeFromText
   in fromMaybeM (InternalError "Client config decode error.") mconfig

fromMaybeErr :: (MonadThrow m, Log m) => Text -> Maybe ErrorCode -> Maybe a -> m a
fromMaybeErr msg errCode =
  fromMaybeM $ ErrorCodeWithMessage msg (fromMaybe CORE001 errCode)
