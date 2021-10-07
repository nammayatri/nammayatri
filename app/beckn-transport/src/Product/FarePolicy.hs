module Product.FarePolicy where

import App.Types
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy as SFarePolicy
import qualified Storage.Queries.Person as SPerson
import Types.API.FarePolicy
import qualified Types.Domain.FarePolicy as DFarePolicy
import Types.Error
import qualified Types.Storage.Person as SP
import Utils.Common (fromMaybeM, withFlowHandlerAPI)

listFarePolicies :: Id SP.Person -> FlowHandler ListFarePolicyResponse
listFarePolicies personId = withFlowHandlerAPI $ do
  person <-
    SPerson.findPersonById personId
      >>= fromMaybeM PersonNotFound
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  farePolicies <- SFarePolicy.findFarePoliciesByOrgId orgId
  pure $ ListFarePolicyResponse $ toResponse <$> farePolicies
  where
    toResponse fp =
      FarePolicyResponse
        { id = fp.id,
          vehicleVariant = fp.vehicleVariant,
          baseFare = fromRational <$> fp.baseFare,
          perExtraKmRateList = DFarePolicy.makeExtraKmRateAPIEntity <$> fp.perExtraKmRateList,
          nightShiftStart = fp.nightShiftStart,
          nightShiftEnd = fp.nightShiftEnd,
          nightShiftRate = fromRational <$> fp.nightShiftRate
        }

updateFarePolicy :: Id SP.Person -> Id DFarePolicy.FarePolicy -> UpdateFarePolicyRequest -> FlowHandler UpdateFarePolicyResponse
updateFarePolicy _ fpId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdateFarePolicyRequest req
  farePolicy <- SFarePolicy.findFarePolicyById fpId >>= fromMaybeM NoFarePolicy
  let updatedFarePolicy =
        farePolicy
          { DFarePolicy.baseFare = toRational <$> req.baseFare,
            DFarePolicy.perExtraKmRateList = DFarePolicy.fromExtraKmRateAPIEntity <$> req.perExtraKmRateList,
            DFarePolicy.nightShiftStart = req.nightShiftStart,
            DFarePolicy.nightShiftEnd = req.nightShiftEnd,
            DFarePolicy.nightShiftRate = toRational <$> req.nightShiftRate
          }
  SFarePolicy.updateFarePolicy updatedFarePolicy
  pure Success
