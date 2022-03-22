module Product.FarePolicy where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import Domain.Types.FarePolicy (makeFarePolicyAPIEntity)
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.FarePolicy.PerExtraKmRate as DPerExtraKmRate
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy as SFarePolicy
import Types.API.FarePolicy
import Types.Error
import Utils.Common (fromMaybeM, throwError, withFlowHandlerAPI)

listFarePolicies :: SP.Person -> FlowHandler ListFarePolicyRes
listFarePolicies person = withFlowHandlerAPI $ do
  let Just orgId = person.organizationId
  farePolicies <- SFarePolicy.findFarePoliciesByOrgId orgId
  pure $ ListFarePolicyRes $ makeFarePolicyAPIEntity <$> farePolicies

updateFarePolicy :: SP.Person -> Id DFarePolicy.FarePolicy -> UpdateFarePolicyReq -> FlowHandler UpdateFarePolicyRes
updateFarePolicy admin fpId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdateFarePolicyRequest req
  farePolicy <- SFarePolicy.findById fpId >>= fromMaybeM NoFarePolicy
  unless (admin.organizationId == Just farePolicy.organizationId) $ throwError AccessDenied
  let perExtraKmRateList = map DPerExtraKmRate.fromPerExtraKmRateAPIEntity req.perExtraKmRateList
  let updatedFarePolicy =
        farePolicy{baseFare = toRational <$> req.baseFare,
                   perExtraKmRateList = perExtraKmRateList,
                   nightShiftStart = req.nightShiftStart,
                   nightShiftEnd = req.nightShiftEnd,
                   nightShiftRate = toRational <$> req.nightShiftRate
                  }
  Esq.runTransaction $
    SFarePolicy.updateFarePolicy updatedFarePolicy
  pure Success
