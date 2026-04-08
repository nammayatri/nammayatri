module Safety.Common.Builder
  ( BuildSafetyCtx (..),
    withSafetyCtx,
  )
where

import Kernel.Prelude hiding (handle)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow)
import Safety.Common.Handle
import Safety.Common.Types
import qualified Safety.Domain.Types.Common as Common

class BuildSafetyCtx authToken m where
  buildSafetyCtx :: SafetyHandle m -> authToken -> m SafetyCtx

-- | Rider: 2-tuple — no MerchantOperatingCity in auth, set to Nothing
instance (MonadFlow m) => BuildSafetyCtx (Id Common.Person, Id Common.Merchant) m where
  buildSafetyCtx handle (personId, merchantId) = do
    let merchantOpCityId = Nothing
    personDefaults <- fetchPersonDefaults handle personId
    pure SafetyCtx {..}

-- | Driver: 3-tuple — opCityId already present, no DB call needed
instance (MonadFlow m) => BuildSafetyCtx (Id Common.Person, Id Common.Merchant, Id Common.MerchantOperatingCity) m where
  buildSafetyCtx _handle (personId, merchantId, rawMerchantOpCityId) = do
    let merchantOpCityId = Just rawMerchantOpCityId
        personDefaults = emptyPersonDefaults
    pure SafetyCtx {..}

-- | Facade: single domain entry point
withSafetyCtx ::
  (BuildSafetyCtx authToken m, Monad m) =>
  SafetyHandle m ->
  authToken ->
  (SafetyCtx -> m a) ->
  m a
withSafetyCtx handle token action =
  buildSafetyCtx handle token >>= action
