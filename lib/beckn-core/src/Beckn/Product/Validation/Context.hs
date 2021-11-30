module Beckn.Product.Validation.Context where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration.Context as Mig
import qualified Beckn.Types.Core.Migration.Domain as Mig
import qualified Beckn.Types.Core.Migration1.Common.Context as Mig1
import Beckn.Types.Error
import Beckn.Types.Field
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateContextMig1 :: (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) => Mig1.Context -> m ()
validateContextMig1 context = do
  validateDomainMig1 Mig1.MOBILITY context
  validateContextCommonsMig1 context

validateDomainMig :: (L.MonadFlow m, Log m) => Mig.Domain -> Mig.Context -> m ()
validateDomainMig expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateDomainMig1 :: (L.MonadFlow m, Log m) => Mig1.Domain -> Mig1.Context -> m ()
validateDomainMig1 expectedDomain context =
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

validateCoreVersionMig1 ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Mig1.Context ->
  m ()
validateCoreVersionMig1 context = do
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

validateContextCommonsMig1 ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Mig1.Context ->
  m ()
validateContextCommonsMig1 context = do
  validateCoreVersionMig1 context
