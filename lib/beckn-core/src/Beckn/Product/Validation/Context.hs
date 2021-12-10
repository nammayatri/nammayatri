module Beckn.Product.Validation.Context where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration.Context as Mig
import qualified Beckn.Types.Core.Migration.Domain as Mig
import qualified Beckn.Types.Core.Taxi.Common.Context as Cab
import Beckn.Types.Error
import Beckn.Types.Field
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateContext :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Cab.Context -> m ()
validateContext context = do
  validateDomain Cab.MOBILITY context
  validateContextCommons context

validateDomainMig :: (L.MonadFlow m, Log m) => Mig.Domain -> Mig.Context -> m ()
validateDomainMig expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateDomain :: (L.MonadFlow m, Log m) => Cab.Domain -> Cab.Context -> m ()
validateDomain expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateCountryMig :: (L.MonadFlow m, Log m) => Mig.Context -> m ()
validateCountryMig context =
  unless (context.country == "IND") $
    throwError InvalidCountry

validateActionMig :: (L.MonadFlow m, Log m) => Mig.Action -> Mig.Context -> m ()
validateActionMig expectedAction context =
  unless (context.action == expectedAction) $
    throwError InvalidAction

validateCoreVersionMig ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Mig.Context ->
  m ()
validateCoreVersionMig context = do
  supportedVersion <- view #coreVersion
  unless (context.core_version == supportedVersion) $
    throwError UnsupportedCoreVer

validateCoreVersion ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Cab.Context ->
  m ()
validateCoreVersion context = do
  supportedVersion <- view #coreVersion
  unless (context.core_version == supportedVersion) $
    throwError UnsupportedCoreVer

validateContextCommonsMig ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Mig.Action ->
  Mig.Context ->
  m ()
validateContextCommonsMig expectedAction context = do
  -- TODO: City validation
  validateActionMig expectedAction context
  validateCoreVersionMig context
  validateCountryMig context

validateContextCommons ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Cab.Context ->
  m ()
validateContextCommons context = do
  validateCoreVersion context
