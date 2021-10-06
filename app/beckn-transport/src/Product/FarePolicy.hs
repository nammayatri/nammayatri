module Product.FarePolicy where

import App.Types
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy as SFarePolicy
import Types.API.FarePolicy
import Types.Domain.FarePolicy (makeFarePolicyAPIEntity)
import qualified Types.Domain.FarePolicy as DFarePolicy
import Types.Error
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
        farePolicy{baseFare = toRational <$> req.baseFare,
                   baseDistance = toRational <$> req.baseDistance,
                   perExtraKmRateList = DFarePolicy.fromExtraKmRateAPIEntity <$> req.perExtraKmRateList,
                   nightShiftStart = req.nightShiftStart,
                   nightShiftEnd = req.nightShiftEnd,
                   nightShiftRate = toRational <$> req.nightShiftRate
                  }
  SFarePolicy.updateFarePolicy updatedFarePolicy
  pure Success
