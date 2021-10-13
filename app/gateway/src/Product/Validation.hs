module Product.Validation where

import App.Types (CoreVersions)
import EulerHS.Prelude
import Temporary.Validation (validateAction, validateCity, validateCountry)
import Types.Beckn.Context
-- import Types.Beckn.Domain
-- import Types.Error
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
