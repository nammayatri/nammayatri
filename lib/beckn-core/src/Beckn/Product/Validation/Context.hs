module Beckn.Product.Validation.Context where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log)
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateDomain :: (L.MonadFlow m, Log m) => Domain -> Context -> m ()
validateDomain expectedDomain context =
  unless (context ^. #_domain == expectedDomain) $
    throwBecknError400 "INVALID_DOMAIN"

validateCountry :: (L.MonadFlow m, Log m) => Context -> m ()
validateCountry context =
  unless (context ^. #_country == Just "IND") $
    throwBecknError400 "INVALID_COUNTRY"

validateCity :: (L.MonadFlow m, Log m) => Context -> m ()
validateCity context =
  -- just for testing purposes, to be rewritten later as well as country check
  unless (isJust $ context ^. #_country) $
    throwBecknError400 "INVALID_CITY"

validateAction :: (L.MonadFlow m, Log m) => Text -> Context -> m ()
validateAction expectedAction context =
  unless (context ^. #_action == expectedAction) $
    throwBecknError400 "INVALID_ACTION"

validateCoreVersion ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Context ->
  m ()
validateCoreVersion context = do
  supportedVersion <- view #coreVersion
  unless (context ^. #_core_version == Just supportedVersion) $
    throwBecknError400 "UNSUPPORTED_CORE_VERSION"

validateDomainVersion ::
  ( HasFlowEnv m r '["domainVersion" ::: Text],
    Log m
  ) =>
  Context ->
  m ()
validateDomainVersion context = do
  supportedVersion <- view #domainVersion
  unless (context ^. #_domain_version == Just supportedVersion) $
    throwBecknError400 "UNSUPPORTED_DOMAIN_VERSION"

validateContextCommons ::
  ( HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text],
    Log m
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
