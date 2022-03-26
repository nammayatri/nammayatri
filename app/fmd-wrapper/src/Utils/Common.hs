module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import Beckn.Types.Common
import Beckn.Utils.Common as CoreCommon
import EulerHS.Prelude
import Types.Error

fromMaybeErr :: (MonadThrow m, Log m) => Text -> Maybe ErrorCode -> Maybe a -> m a
fromMaybeErr msg errCode =
  fromMaybeM $ ErrorCodeWithMessage msg (fromMaybe CORE001 errCode)
