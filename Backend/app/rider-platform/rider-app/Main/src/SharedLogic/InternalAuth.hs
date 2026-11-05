module SharedLogic.InternalAuth
  ( checkInternalKey,
    checkLtsKey,
    requireInternalKey,
  )
where

import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

checkInternalKey ::
  (MonadFlow m, HasFlowEnv m r '["enforcingInternalAuth" ::: Bool]) =>
  Text ->
  [Text] ->
  Maybe Text ->
  m ()
checkInternalKey route acceptedKeys mbKey = do
  enforcing <- asks (.enforcingInternalAuth)
  unless (maybe False (`elem` acceptedKeys) mbKey) $
    if enforcing
      then throwError $ AuthBlocked "Invalid internal api key"
      else logWarning $ "InternalAuth: missing or invalid key on " <> route

checkLtsKey ::
  (MonadFlow m, HasFlowEnv m r '["enforcingInternalAuth" ::: Bool, "locationTrackingServiceKey" ::: Text]) =>
  Text ->
  Maybe Text ->
  m ()
checkLtsKey route mbKey = do
  ltsKey <- asks (.locationTrackingServiceKey)
  checkInternalKey route [ltsKey] mbKey

requireInternalKey :: (MonadFlow m) => [Text] -> Maybe Text -> m ()
requireInternalKey acceptedKeys mbKey =
  unless (maybe False (`elem` acceptedKeys) mbKey) $
    throwError $ AuthBlocked "Invalid internal api key"
