{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ConfigPilot interface: type-safe link between ConfigType and dimension records.
--
-- This module contains ONLY class definitions and type-level machinery.
-- All instances are defined in Storage.ConfigPilot.Config.* modules.
--
-- **Adding a new config type (3 steps):**
-- 1. Add dimension type in @Storage.ConfigPilot.Config.<Name>.hs@ (one record type).
-- 2. Add a constructor to the @SConfigType@ GADT below (one line).
-- 3. Add an instance of @ConfigTypeInfo@ for that config in the appropriate Config module.
--
-- Reuse an existing dimension type (e.g. CommonDimensions) by using it in step 3.
module Lib.ConfigPilot.Interface.Types
  ( -- * Type Classes
    ConfigTypeInfo (..),
    ConfigDimensions (..),
    ToMaybeOne (..),
    getOneConfig,

    -- * Singleton GADT
    SConfigType (..),
    sConfigTypeToConfigType,

    -- * Cache key generics (needed for default dimensionsCacheKey)
    GNonMaybeFields (..),
    CacheKeyVal (..),
  )
where

import qualified Data.Text as T
import GHC.Generics (C, D, Generic (from), K1 (..), M1 (..), Rep, S, (:*:) (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (incrementConfigPilotFailureCounter, incrementConfigPilotSuccessCounter)
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, logDebug, logError, throwError, withLogTag)
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import System.Environment (lookupEnv)
import Prelude (id)

-- -----------------------------------------------------------------------------
-- Single source of truth per config: dimension type + singleton + term-level ConfigType
-- -----------------------------------------------------------------------------

class
  ( ToJSON (DimensionsFor cfg),
    FromJSON (DimensionsFor cfg),
    Show (DimensionsFor cfg),
    Eq (DimensionsFor cfg)
  ) =>
  ConfigTypeInfo (cfg :: ConfigType)
  where
  type DimensionsFor cfg :: Type
  configTypeValue :: ConfigType
  sConfigType :: SConfigType cfg

-- | Singleton: bridges term-level ConfigType to type-level (used in FindRequest).
data SConfigType (cfg :: ConfigType) where
  STransporterConfig :: SConfigType 'TransporterConfig
  SDriverPoolConfig :: SConfigType 'DriverPoolConfig
  SRiderConfig :: SConfigType 'RiderConfig
  SFRFSConfig :: SConfigType 'FRFSConfig
  SPayoutConfig :: SConfigType 'PayoutConfig
  SMerchantServiceUsageConfigDriver :: SConfigType 'MerchantServiceUsageConfigDriver
  SMerchantServiceUsageConfigRider :: SConfigType 'MerchantServiceUsageConfigRider
  SHotSpotConfig :: SConfigType 'HotSpotConfig
  SMerchantConfig :: SConfigType 'MerchantConfig
  SRideRelatedNotificationConfig :: SConfigType 'RideRelatedNotificationConfig
  SMerchantMessage :: SConfigType 'MerchantMessage
  SMerchantPushNotification :: SConfigType 'MerchantPushNotification
  SDriverIntelligentPoolConfig :: SConfigType 'DriverIntelligentPoolConfig
  SLeaderBoardConfig :: SConfigType 'LeaderBoardConfig
  SCoinsConfig :: SConfigType 'CoinsConfig
  SDocumentVerificationConfig :: SConfigType 'DocumentVerificationConfig
  SFleetOwnerDocumentVerificationConfig :: SConfigType 'FleetOwnerDocumentVerificationConfig
  SGoHomeConfig :: SConfigType 'GoHomeConfig
  SSubscriptionConfig :: SConfigType 'SubscriptionConfig
  SOverlay :: SConfigType 'Overlay
  SFarePolicy :: SConfigType 'FarePolicy
  SFareProduct :: SConfigType 'FareProduct
  SPlan :: SConfigType 'Plan
  SPlanTranslation :: SConfigType 'PlanTranslation
  SVehicleServiceTier :: SConfigType 'VehicleServiceTier
  SToll :: SConfigType 'Toll
  SCancellationFarePolicy :: SConfigType 'CancellationFarePolicy
  SSurgePricing :: SConfigType 'SurgePricing
  SBecknConfig :: SConfigType 'BecknConfig
  SMerchantServiceConfig :: SConfigType 'MerchantServiceConfig
  SMerchantServiceConfigDriver :: SConfigType 'MerchantServiceConfigDriver
  SExophone :: SConfigType 'Exophone
  SInsuranceConfig :: SConfigType 'InsuranceConfig
  SVehicleConfig :: SConfigType 'VehicleConfig
  SExophoneRider :: SConfigType 'ExophoneRider
  SPayoutConfigRider :: SConfigType 'PayoutConfigRider
  SMerchantPushNotificationRider :: SConfigType 'MerchantPushNotificationRider
  SRideRelatedNotificationConfigRider :: SConfigType 'RideRelatedNotificationConfigRider
  SScheduledPayoutConfig :: SConfigType 'ScheduledPayoutConfig
  SReminderConfig :: SConfigType 'ReminderConfig
  STagActionNotificationConfig :: SConfigType 'TagActionNotificationConfig
  SUiDriverConfig :: SConfigType 'UiDriverConfig
  STranslationDriver :: SConfigType 'TranslationDriver
  STranslationRider :: SConfigType 'TranslationRider
  SCancellationReason :: SConfigType 'CancellationReason
  SMerchantPaymentMethod :: SConfigType 'MerchantPaymentMethod
  SPassCategory :: SConfigType 'PassCategory
  SIntegratedBPPConfig :: SConfigType 'IntegratedBPPConfig
  SIssueConfigRider :: SConfigType 'IssueConfigRider
  SIssueConfigDriver :: SConfigType 'IssueConfigDriver
  SMonetaryRewardConfig :: SConfigType 'MonetaryRewardConfig

deriving instance Show (SConfigType cfg)

deriving instance Eq (SConfigType cfg)

-- | Convert singleton back to term-level ConfigType (one line via class).
sConfigTypeToConfigType :: forall cfg. ConfigTypeInfo cfg => SConfigType cfg -> ConfigType
sConfigTypeToConfigType _ = configTypeValue @cfg

-- -----------------------------------------------------------------------------
-- ConfigDimensions: dimension record @a@ with ConfigType, config value type, and getConfig
-- -----------------------------------------------------------------------------

class (Show a, ConfigTypeInfo (ConfigTypeOf a)) => ConfigDimensions a where
  type ConfigTypeOf a :: ConfigType
  type ConfigValueTypeOf a :: Type
  getConfigType :: a -> ConfigType

  dimensionsCacheKey :: a -> Text
  default dimensionsCacheKey :: (Generic a, GNonMaybeFields (Rep a)) => a -> Text
  dimensionsCacheKey = T.intercalate ":" . gNonMaybeFields . from

  -- | Fetch all configs (no dimension filtering). Instances implement this.
  getConfigList :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => a -> m (ConfigValueTypeOf a)
  getConfigList _ = throwError $ InvalidRequest "getConfigList not implemented for this dimension"

  -- | Apply dimension-based filtering. Default: identity (no filtering).
  filterByDimensions :: a -> ConfigValueTypeOf a -> ConfigValueTypeOf a
  filterByDimensions _ = id

  -- | Fetch filtered configs with an optional fallback action triggered on any exception.
  -- Wraps 'getConfigList' in a try block with structured logging under
  -- @CONFIG_PILOT:<POD_NAME>@ tag. Entry and exit are logged at debug level.
  -- On success the @config_pilot_success_metric@ counter is incremented; on failure
  -- the @config_pilot_failure_metric@ counter is incremented (both labelled by table name).
  -- When a fallback is provided ('Just'), failures log an error and invoke it;
  -- with 'Nothing' the original exception is re-thrown after the failure metric is recorded.
  getConfig ::
    (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
    a ->
    Maybe (m (ConfigValueTypeOf a)) ->
    m (ConfigValueTypeOf a)
  getConfig dims mbFallback = do
    let tableName = show (getConfigType dims)
    disableConfigPilot <- liftIO $ maybe False read <$> lookupEnv "DISABLE_CONFIG_PILOT"
    withLogTag "CONFIG_PILOT:" $
      if disableConfigPilot
        then fromMaybe (throwM $ InternalError $ "No Fallback configured for table: " <> tableName) mbFallback
        else do
          logDebug $ "getConfig:entry table=" <> tableName <> " dims=" <> show dims
          let onSuccess cfg = do
                incrementConfigPilotSuccessCounter tableName
                pure cfg
              onFailure (e :: SomeException) = do
                logError $ "Fetch failed from config pilot, triggering fallback. error=" <> show e
                incrementConfigPilotFailureCounter tableName
                fromMaybe (throwM e) mbFallback
          result <-
            handle onFailure $
              (filterByDimensions dims <$> getConfigList dims) >>= onSuccess
          logDebug $ "getConfig:exit table=" <> tableName <> " dims=" <> show dims
          pure result

  setConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => a -> m (ConfigValueTypeOf a)
  setConfig _ = throwError $ InvalidRequest "setConfig not implemented for this dimension"

-- | Extract a single element from either @Maybe a@ or @[a]@, erroring if more than one found.
class ToMaybeOne f where
  type ElemOf f
  toMaybeOne :: MonadFlow m => Text -> f -> m (Maybe (ElemOf f))

instance ToMaybeOne (Maybe a) where
  type ElemOf (Maybe a) = a
  toMaybeOne _ = pure

instance ToMaybeOne [a] where
  type ElemOf [a] = a
  toMaybeOne _ [] = pure Nothing
  toMaybeOne _ [x] = pure (Just x)
  toMaybeOne dimInfo (_ : _) =
    throwError $ InvalidRequest $ "Expected at most one config row but found multiple for dimensions: " <> dimInfo

-- | Fetch a single config value after dimension filtering. Works for both @Maybe@ and @[]@ return types.
-- Throws an error if multiple rows match the given dimensions.
-- NOINLINE: prevents GHC from inlining this into every call site (e.g. Search.hs calls it 3x
-- with TransporterConfig which has 390+ fields), which causes simplifier ticks exhaustion at -O2.
{-# NOINLINE getOneConfig #-}
getOneConfig ::
  (ConfigDimensions a, ToMaybeOne (ConfigValueTypeOf a), Show a, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  a ->
  Maybe (m (ConfigValueTypeOf a)) ->
  m (Maybe (ElemOf (ConfigValueTypeOf a)))
getOneConfig dims mbFallback = getConfig dims mbFallback >>= toMaybeOne (show dims)

-- -----------------------------------------------------------------------------
-- Generic machinery: extract non-Maybe field values as cache key parts
-- -----------------------------------------------------------------------------

-- | Convert a value to a cache key part. Text is used as-is; other types use 'show'.
class CacheKeyVal a where
  cacheKeyVal :: a -> Text

instance CacheKeyVal Text where
  {-# INLINE cacheKeyVal #-}
  cacheKeyVal = id

instance {-# OVERLAPPABLE #-} Show a => CacheKeyVal a where
  {-# INLINE cacheKeyVal #-}
  cacheKeyVal = show

-- | Generic traversal that collects non-Maybe field values.
class GNonMaybeFields f where
  gNonMaybeFields :: f p -> [Text]

instance GNonMaybeFields f => GNonMaybeFields (M1 D c f) where
  {-# INLINE gNonMaybeFields #-}
  gNonMaybeFields (M1 x) = gNonMaybeFields x

instance GNonMaybeFields f => GNonMaybeFields (M1 C c f) where
  {-# INLINE gNonMaybeFields #-}
  gNonMaybeFields (M1 x) = gNonMaybeFields x

instance (GNonMaybeFields f, GNonMaybeFields g) => GNonMaybeFields (f :*: g) where
  {-# INLINE gNonMaybeFields #-}
  gNonMaybeFields (f :*: g) = gNonMaybeFields f <> gNonMaybeFields g

-- | Maybe fields: include unwrapped value when Just, skip when Nothing.
instance {-# OVERLAPPING #-} CacheKeyVal a => GNonMaybeFields (M1 S c (K1 i (Maybe a))) where
  {-# INLINE gNonMaybeFields #-}
  gNonMaybeFields (M1 (K1 Nothing)) = []
  gNonMaybeFields (M1 (K1 (Just x))) = [cacheKeyVal x]

-- | Include non-Maybe fields in the cache key.
instance {-# OVERLAPPABLE #-} CacheKeyVal a => GNonMaybeFields (M1 S c (K1 i a)) where
  {-# INLINE gNonMaybeFields #-}
  gNonMaybeFields (M1 (K1 x)) = [cacheKeyVal x]
