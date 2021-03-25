module Beckn.Product.Validation.Context where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log)
import qualified EulerHS.Language as L
import EulerHS.Prelude

validateDomain :: (L.MonadFlow m, Log m) => Domain -> Context -> m ()
validateDomain expectedDomain context =
  unless (context ^. #_domain == expectedDomain) $
    throwError400 InvalidDomain

validateCountry :: (L.MonadFlow m, Log m) => Context -> m ()
validateCountry context =
  unless (context ^. #_country == Just "IND") $
    throwError400 InvalidCountry

validateCity :: (L.MonadFlow m, Log m) => Context -> m ()
validateCity context =
  -- just for testing purposes, to be rewritten later as well as country check
  unless (isJust $ context ^. #_country) $
    throwError400 InvalidCity

validateAction :: (L.MonadFlow m, Log m) => Text -> Context -> m ()
validateAction expectedAction context =
  unless (context ^. #_action == expectedAction) $
    throwError400 InvalidAction

validateCoreVersion ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    Log m
  ) =>
  Context ->
  m ()
validateCoreVersion context = do
  supportedVersion <- view #coreVersion
  unless (context ^. #_core_version == Just supportedVersion) $
    throwError400 UnsupportedCoreVer

validateDomainVersion ::
  ( HasFlowEnv m r '["domainVersion" ::: Text],
    Log m
  ) =>
  Context ->
  m ()
validateDomainVersion context = do
  supportedVersion <- view #domainVersion
  unless (context ^. #_domain_version == Just supportedVersion) $
    throwError400 UnsupportedDomainVer

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
