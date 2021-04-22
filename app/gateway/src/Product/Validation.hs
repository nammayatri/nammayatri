module Product.Validation where

import App.Types
import Beckn.Product.Validation.Context (validateAction, validateCity, validateCountry)
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Utils.Common
import EulerHS.Prelude
import Types.Error

validateContext :: Text -> Context -> Flow ()
validateContext expectedAction context = do
  validateAction expectedAction context
  validateCountry context
  validateCity context
  validateVersion context

validateVersion :: Context -> Flow ()
validateVersion context = do
  let domain = context ^. #_domain
  (desiredCoreVersion, desiredDomainVersion) <-
    case domain of
      MOBILITY -> do
        mobilityCoreVersion <- view #mobilityCoreVersion
        mobilityDomainVersion <- view #mobilityDomainVersion
        return (Just mobilityCoreVersion, Just mobilityDomainVersion)
      FINAL_MILE_DELIVERY -> do
        fmdCoreVersion <- view #fmdCoreVersion
        fmdDomainVersion <- view #fmdDomainVersion
        return (Just fmdCoreVersion, Just fmdDomainVersion)
      -- TODO: validate for these domains when enabled
      FOOD_AND_BEVERAGE -> return (Nothing, Nothing)
      HEALTHCARE -> return (Nothing, Nothing)
  unless (context ^. #_core_version == desiredCoreVersion) $
    throwError UnsupportedCoreVer
  unless (context ^. #_domain_version == desiredDomainVersion) $
    throwError UnsupportedDomainVer
