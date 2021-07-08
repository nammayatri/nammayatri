module Product.Validation where

import EulerHS.Prelude
import Temporary.Validation (validateAction, validateCity, validateCountry)
import Types.Beckn.Context
import Types.Beckn.Domain
import Types.Error
import Utils.Common

validateContext ::
  ( HasFlowEnv m r ["mobilityCoreVersion" ::: Text, "mobilityDomainVersion" ::: Text],
    HasFlowEnv m r '["fmdCoreVersion" ::: Text],
    HasFlowEnv m r '["localRetailCoreVersion" ::: Text],
    HasFlowEnv m r '["foodAndBeverageCoreVersion" ::: Text]
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
  ( HasFlowEnv m r ["mobilityCoreVersion" ::: Text, "mobilityDomainVersion" ::: Text],
    HasFlowEnv m r '["fmdCoreVersion" ::: Text],
    HasFlowEnv m r '["localRetailCoreVersion" ::: Text],
    HasFlowEnv m r '["foodAndBeverageCoreVersion" ::: Text]
  ) =>
  Context ->
  m ()
validateVersion context = do
  let domain = context.domain
  (desiredCoreVersion, desiredDomainVersion) <-
    case domain of
      MOBILITY -> do
        mobilityCoreVersion <- asks (.mobilityCoreVersion)
        mobilityDomainVersion <- asks (.mobilityDomainVersion)
        return (Just mobilityCoreVersion, Just mobilityDomainVersion)
      FINAL_MILE_DELIVERY -> do
        fmdCoreVersion <- asks (.fmdCoreVersion)
        return (Just fmdCoreVersion, Nothing)
      -- TODO: validate for these domains when enabled
      LOCAL_RETAIL -> do
        localRetailCoreVersion <- asks (.localRetailCoreVersion)
        return (Just localRetailCoreVersion, Nothing)
      FOOD_AND_BEVERAGE -> do
        foodAndBeverageCoreVersion <- asks (.foodAndBeverageCoreVersion)
        return (Just foodAndBeverageCoreVersion, Nothing)
      HEALTHCARE -> return (Nothing, Nothing)
  unless (context.core_version == desiredCoreVersion) $
    throwError UnsupportedCoreVer
  unless (context.domain_version == desiredDomainVersion) $
    throwError UnsupportedDomainVer
