module Product.Validation where

import App.Types (CoreVersions)
import EulerHS.Prelude
import Temporary.Validation (validateAction, validateCity, validateCountry)
import Types.Beckn.Context
import Types.Beckn.Domain
import Types.Error
import Utils.Common

validateContext ::
  ( HasFlowEnv m r ["coreVersions" ::: CoreVersions, "mobilityDomainVersion" ::: Text]
  ) =>
  Text ->
  Context ->
  m ()
validateContext expectedAction context = do
  validateAction expectedAction context
  validateCountry context
  validateCity context
  validateVersion context

validateVersion ::
  ( HasFlowEnv m r ["coreVersions" ::: CoreVersions, "mobilityDomainVersion" ::: Text]
  ) =>
  Context ->
  m ()
validateVersion context = do
  let domain = context.domain
  desiredCoreVersion <-
    case domain of
      MOBILITY -> do
        mobilityDomainVersion <- asks (.mobilityDomainVersion)
        unless (context.domain_version == Just mobilityDomainVersion) $
          throwError UnsupportedDomainVer
        asks (.coreVersions.mobility)
      FINAL_MILE_DELIVERY -> asks (.coreVersions.finalMileDelivery)
      LOCAL_RETAIL -> asks (.coreVersions.localRetail)
      FOOD_AND_BEVERAGE -> asks (.coreVersions.foodAndBeverage)
      HEALTHCARE -> throwError InvalidDomain -- Disabled
  unless (context.core_version == Just desiredCoreVersion) $
    throwError UnsupportedCoreVer
