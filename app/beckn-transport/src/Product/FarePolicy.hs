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
import qualified Types.Domain.FarePolicy.Discount as DFPDiscount
import Types.Error
import qualified Types.Storage.Person as SP
import Utils.Common (fromMaybeM, throwError, withFlowHandlerAPI)

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
          discountList = DFPDiscount.makeDiscountAPIEntity <$> fp.discountList,
          nightShiftStart = fp.nightShiftStart,
          nightShiftEnd = fp.nightShiftEnd,
          nightShiftRate = fromRational <$> fp.nightShiftRate
        }

updateFarePolicy :: SP.Person -> Id DFarePolicy.FarePolicy -> UpdateFarePolicyRequest -> FlowHandler UpdateFarePolicyResponse
updateFarePolicy admin fpId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdateFarePolicyRequest req
  farePolicy <- SFarePolicy.findFarePolicyById fpId >>= fromMaybeM NoFarePolicy
  unless (admin.organizationId == Just farePolicy.organizationId) $ throwError AccessDenied
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
