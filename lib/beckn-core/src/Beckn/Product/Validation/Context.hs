module Beckn.Product.Validation.Context where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateDomain :: L.MonadFlow m => Domain -> Context -> m ()
validateDomain expectedDomain context =
  unless (context ^. #_domain == expectedDomain) $
    throwJsonError400 "validation" "INVALID_DOMAIN"

validateCountry :: L.MonadFlow m => Context -> m ()
validateCountry context =
  unless (context ^. #_country == Just "IND") $
    throwJsonError400 "validation" "INVALID_COUNTRY"

validateCity :: L.MonadFlow m => Context -> m ()
validateCity context =
  -- just for testing purposes, to be rewritten later as well as country check
  unless (isJust $ context ^. #_country) $
    throwJsonError400 "validation" "INVALID_CITY"

validateAction :: L.MonadFlow m => Text -> Context -> m ()
validateAction expectedAction context =
  unless (context ^. #_action == expectedAction) $
    throwJsonError400 "validation" "INVALID_ACTION"

validateCoreVersion ::
  HasFlowEnv m r '["coreVersion" ::: Text] =>
  Context ->
  m ()
validateCoreVersion context = do
  supportedVersion <- view #coreVersion
  unless (context ^. #_core_version == Just supportedVersion) $
    throwJsonError400 "validation" "UNSUPPORTED_CORE_VERSION"

validateDomainVersion ::
  HasFlowEnv m r '["domainVersion" ::: Text] =>
  Context ->
  m ()
validateDomainVersion context = do
  supportedVersion <- view #domainVersion
  unless (context ^. #_domain_version == Just supportedVersion) $
    throwJsonError400 "validation" "UNSUPPORTED_DOMAIN_VERSION"

validateContextCommons ::
  HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text] =>
  Text ->
  Context ->
  m ()
validateContextCommons expectedAction context = do
  validateAction expectedAction context
  validateCoreVersion context
  validateDomainVersion context
  validateCountry context
  validateCity context
