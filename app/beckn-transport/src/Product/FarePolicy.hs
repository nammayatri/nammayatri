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
import Utils.Common (fromMaybeM, throwError, withFlowHandlerAPI)

listFarePolicies :: SP.Person -> FlowHandler ListFarePolicyRes
listFarePolicies person = withFlowHandlerAPI $ do
  let Just orgId = person.organizationId
  farePolicies <- SFarePolicy.findFarePoliciesByOrgId orgId
  pure $ ListFarePolicyRes $ makeFarePolicyAPIEntity <$> farePolicies

updateFarePolicy :: SP.Person -> Id DFarePolicy.FarePolicy -> UpdateFarePolicyReq -> FlowHandler UpdateFarePolicyRes
updateFarePolicy admin fpId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdateFarePolicyRequest req
  farePolicy <- SFarePolicy.findFarePolicyById fpId >>= fromMaybeM NoFarePolicy
  unless (admin.organizationId == Just farePolicy.organizationId) $ throwError AccessDenied
  let updatedFarePolicy =
        farePolicy{baseFare = toRational <$> req.baseFare,
                   perExtraKmRateList = DFarePolicy.fromExtraKmRateAPIEntity <$> req.perExtraKmRateList,
                   nightShiftStart = req.nightShiftStart,
                   nightShiftEnd = req.nightShiftEnd,
                   nightShiftRate = toRational <$> req.nightShiftRate
                  }
  SFarePolicy.updateFarePolicy updatedFarePolicy
  pure Success
