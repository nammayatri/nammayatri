{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
module Storage.ConfigPilot.Interface.Types
  ( -- * Type Classes
    ConfigTypeInfo (..),
    ConfigDimensions (..),
    ToMaybeOne (..),
    getOneConfig,

    -- * Singleton GADT
    SConfigType (..),
    sConfigTypeToConfigType,
  )
where

import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, throwError)
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
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
  SDriverPoolConfig :: SConfigType 'DriverPoolConfig
  SRiderConfig :: SConfigType 'RiderConfig
  SFRFSConfig :: SConfigType 'FRFSConfig
  SPayoutConfig :: SConfigType 'PayoutConfig
  SMerchantServiceUsageConfig :: SConfigType 'MerchantServiceUsageConfig
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
  SExophone :: SConfigType 'Exophone

deriving instance Show (SConfigType cfg)

deriving instance Eq (SConfigType cfg)

-- | Convert singleton back to term-level ConfigType (one line via class).
sConfigTypeToConfigType :: forall cfg. ConfigTypeInfo cfg => SConfigType cfg -> ConfigType
sConfigTypeToConfigType _ = configTypeValue @cfg

-- -----------------------------------------------------------------------------
-- ConfigDimensions: dimension record @a@ with ConfigType, config value type, and getConfig
-- -----------------------------------------------------------------------------

class ConfigTypeInfo (ConfigTypeOf a) => ConfigDimensions a where
  type ConfigTypeOf a :: ConfigType
  type ConfigValueTypeOf a :: Type
  getConfigType :: a -> ConfigType

  -- | Fetch all configs (no dimension filtering). Instances implement this.
  getConfigList :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => a -> m (ConfigValueTypeOf a)
  getConfigList _ = throwError $ InvalidRequest "getConfigList not implemented for this dimension"

  -- | Apply dimension-based filtering. Default: identity (no filtering).
  filterByDimensions :: a -> ConfigValueTypeOf a -> ConfigValueTypeOf a
  filterByDimensions _ = id

  -- | Fetch filtered configs. Default chains getConfigList + filterByDimensions.
  getConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => a -> m (ConfigValueTypeOf a)
  getConfig dims = filterByDimensions dims <$> getConfigList dims

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
getOneConfig ::
  (ConfigDimensions a, ToMaybeOne (ConfigValueTypeOf a), Show a, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  a ->
  m (Maybe (ElemOf (ConfigValueTypeOf a)))
getOneConfig dims = getConfig dims >>= toMaybeOne (show dims)
