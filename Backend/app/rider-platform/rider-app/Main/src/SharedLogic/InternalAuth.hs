module SharedLogic.InternalAuth
  ( checkInternalKey,
    checkLtsKey,
    isValidKey,
    isValidLtsKey,
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
checkInternalKey route acceptedKeys mbKey =
  unless (maybe False (`elem` acceptedKeys) mbKey) $
    rejectInvalidKey route

checkLtsKey ::
  (MonadFlow m, HasFlowEnv m r '["enforcingInternalAuth" ::: Bool, "locationTrackingServiceKey" ::: Text, "locationTrackingServiceOldKeys" ::: [Text]]) =>
  Text ->
  Maybe Text ->
  m ()
checkLtsKey route mbKey = do
  valid <- isValidLtsKey route mbKey
  unless valid $ rejectInvalidKey route

isValidLtsKey ::
  (MonadFlow m, HasFlowEnv m r '["locationTrackingServiceKey" ::: Text, "locationTrackingServiceOldKeys" ::: [Text]]) =>
  Text ->
  Maybe Text ->
  m Bool
isValidLtsKey route mbKey = do
  currentKey <- asks (.locationTrackingServiceKey)
  oldKeys <- asks (.locationTrackingServiceOldKeys)
  isValidKey route currentKey oldKeys mbKey

isValidKey :: (MonadFlow m) => Text -> Text -> [Text] -> Maybe Text -> m Bool
isValidKey route currentKey oldKeys mbKey =
  case mbKey of
    Just key
      | key == currentKey -> pure True
      | key `elem` oldKeys -> do
        logError $ "InternalAuth: old key used on " <> route
        pure True
    _ -> pure False

rejectInvalidKey ::
  (MonadFlow m, HasFlowEnv m r '["enforcingInternalAuth" ::: Bool]) =>
  Text ->
  m ()
rejectInvalidKey route = do
  enforcing <- asks (.enforcingInternalAuth)
  if enforcing
    then throwError $ AuthBlocked "Invalid internal api key"
    else logWarning $ "InternalAuth: missing or invalid key on " <> route

requireInternalKey :: (MonadFlow m) => [Text] -> Maybe Text -> m ()
requireInternalKey acceptedKeys mbKey =
  unless (maybe False (`elem` acceptedKeys) mbKey) $
    throwError $ AuthBlocked "Invalid internal api key"
