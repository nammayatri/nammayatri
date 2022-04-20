module Product.FarePolicy where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.FarePolicy.PerExtraKmRate as DPerExtraKmRate
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy as SFarePolicy
import qualified Storage.Queries.Person as QP
import Types.API.FarePolicy
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

listFarePolicies :: SP.Person -> FlowHandler ListFarePolicyRes
listFarePolicies person = withFlowHandlerAPI $ do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  oneWayFarePolicies <- SFarePolicy.findFarePoliciesByOrgId orgId
  pure $
    ListFarePolicyRes
      { oneWayFarePolicies = map makeFarePolicyAPIEntity oneWayFarePolicies
      }

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
  let Just orgId = admin.organizationId
  coordinators <- QP.findAdminsByOrgId orgId
  Esq.runTransaction $
    SFarePolicy.updateFarePolicy updatedFarePolicy
  let otherCoordinators = filter (\coordinator -> coordinator.id /= admin.id) coordinators
  for_ otherCoordinators $ \cooridinator -> do
    Notify.notifyFarePolicyChange cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateFarePolicy : ") (show updatedFarePolicy)
  pure Success
