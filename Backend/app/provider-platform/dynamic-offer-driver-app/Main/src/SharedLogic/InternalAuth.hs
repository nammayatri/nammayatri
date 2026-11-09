module SharedLogic.InternalAuth
  ( checkInternalKey,
    checkLtsKey,
    checkListKey,
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

checkListKey :: Text -> Id Merchant -> Maybe Text -> Flow ()
checkListKey route merchantId apiKey = do
  ltsKey <- asks (.locationTrackingServiceKey)
  mbMerchant <- QM.findById merchantId
  checkInternalKey route (ltsKey : maybe [] (\m -> [m.internalApiKey]) mbMerchant) apiKey

requireMerchantKey :: Id Merchant -> Maybe Text -> Flow ()
requireMerchantKey merchantId apiKey = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  requireInternalKey [merchant.internalApiKey] apiKey
