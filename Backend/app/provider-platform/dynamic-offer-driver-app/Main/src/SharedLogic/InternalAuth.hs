module SharedLogic.InternalAuth
  ( checkInternalKey,
    checkLtsKey,
    checkListKey,
    isValidKey,
    isValidLtsKey,
    requireMerchantKey,
    requireInternalKey,
  )
where

import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as Fields
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM

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

checkListKey :: Text -> Id Merchant -> Maybe Text -> Flow ()
checkListKey route merchantId apiKey = do
  validLtsKey <- isValidLtsKey route apiKey
  unless validLtsKey $ do
    mbMerchant <- QM.findById merchantId
    checkInternalKey route (maybe [] (\m -> [m.internalApiKey]) mbMerchant) apiKey

requireMerchantKey :: Id Merchant -> Maybe Text -> Flow ()
requireMerchantKey merchantId apiKey = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  requireInternalKey [merchant.internalApiKey] apiKey
