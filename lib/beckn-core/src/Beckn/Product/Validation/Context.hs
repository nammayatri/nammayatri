module Beckn.Product.Validation.Context where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import qualified Beckn.Types.Core.Migration.Context as Mig
import qualified Beckn.Types.Core.Migration.Domain as Mig
import Beckn.Types.Error
import Beckn.Types.Field
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateDomain :: (L.MonadFlow m, Log m) => Domain -> Context -> m ()
validateDomain expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateDomainMig :: (L.MonadFlow m, Log m) => Mig.Domain -> Mig.Context -> m ()
validateDomainMig expectedDomain context =
  unless (context.domain == expectedDomain) $
    throwError InvalidDomain

validateCountry :: (L.MonadFlow m, Log m) => Context -> m ()
validateCountry context =
  unless (context.country == Just "IND") $
    throwError InvalidCountry

validateCountryMig :: (L.MonadFlow m, Log m) => Mig.Context -> m ()
validateCountryMig context =
  unless (context.country == "IND") $
    throwError InvalidCountry

validateCity :: (L.MonadFlow m, Log m) => Context -> m ()
validateCity context =
  -- just for testing purposes, to be rewritten later as well as country check
  unless (isJust $ context.country) $
    throwError InvalidCity

validateAction :: (L.MonadFlow m, Log m) => Text -> Context -> m ()
validateAction expectedAction context =
  unless (context.action == expectedAction) $
    throwError InvalidAction

validateActionMig :: (L.MonadFlow m, Log m) => Mig.Action -> Mig.Context -> m ()
validateActionMig expectedAction context =
  unless (context.action == expectedAction) $
    throwError InvalidAction

validateCoreVersion ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  Context ->
  m ()
validateCoreVersion context = do
  supportedVersion <- view #coreVersion
  unless (context.core_version == Just supportedVersion) $
    throwError UnsupportedCoreVer

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

validateDomainVersion ::
  ( HasFlowEnv m r '["domainVersion" ::: Text]
  ) =>
  Context ->
  m ()
validateDomainVersion context = do
  supportedVersion <- view #domainVersion
  unless (context.domain_version == Just supportedVersion) $
    throwError UnsupportedDomainVer

validateContextCommons ::
  ( HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]
  ) =>
  Text ->
  Context ->
  m ()
validateContextCommons expectedAction context = do
  validateAction expectedAction context
  validateCoreVersion context
  validateDomainVersion context
  validateCountry context
  validateCity context

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
