module Product.FarePolicy where

import App.Types
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy as SFarePolicy
import Types.API.FarePolicy
import qualified Types.Domain.FarePolicy as DFarePolicy
import Types.Error
import Types.Storage.FarePolicy (makeFarePolicyAPIEntity)
import qualified Types.Storage.Person as SP
import Utils.Common (fromMaybeM, withFlowHandlerAPI)

listFarePolicies :: SP.Person -> FlowHandler ListFarePolicyRes
listFarePolicies person = withFlowHandlerAPI $ do
  let Just orgId = person.organizationId
  farePolicies <- SFarePolicy.findFarePoliciesByOrgId orgId
  pure $ ListFarePolicyRes $ makeFarePolicyAPIEntity <$> farePolicies

updateFarePolicy :: Id SP.Person -> Id DFarePolicy.FarePolicy -> UpdateFarePolicyReq -> FlowHandler UpdateFarePolicyRes
updateFarePolicy _ fpId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdateFarePolicyRequest req
  farePolicy <- SFarePolicy.findFarePolicyById fpId >>= fromMaybeM NoFarePolicy
  let updatedFarePolicy =
        farePolicy{baseFare = req.baseFare,
                   baseDistance = req.baseDistance,
                   perExtraKmRate = req.perExtraKmRate,
                   nightShiftStart = req.nightShiftStart,
                   nightShiftEnd = req.nightShiftEnd,
                   nightShiftRate = req.nightShiftRate
                  }
  SFarePolicy.updateFarePolicy updatedFarePolicy
  pure Success
