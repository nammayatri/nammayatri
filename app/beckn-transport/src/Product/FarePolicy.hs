module Product.FarePolicy where

import App.Types
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import qualified Beckn.Types.Storage.RegistrationToken as RegToken
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy as SFarePolicy
import qualified Storage.Queries.Person as SPerson
import Types.API.FarePolicy
  ( FarePolicyResponse (..),
    ListFarePolicyResponse (ListFarePolicyResponse),
    UpdateFarePolicyRequest,
    UpdateFarePolicyResponse,
  )
import qualified Types.Domain.FarePolicy as DFarePolicy
import Types.Error
import qualified Types.Storage.FarePolicy as SFarePolicy
import Utils.Common (fromMaybeM, withFlowHandlerAPI)

listFarePolicies :: RegToken.RegistrationToken -> FlowHandler ListFarePolicyResponse
listFarePolicies RegToken.RegistrationToken {entityId} = withFlowHandlerAPI $ do
  person <-
    SPerson.findPersonById (Id entityId)
      >>= fromMaybeM PersonNotFound
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  farePolicies <- SFarePolicy.findFarePoliciesByOrgId orgId
  pure $ ListFarePolicyResponse $ toResponse <$> farePolicies
  where
    toResponse fp =
      FarePolicyResponse
        { id = fp.id,
          vehicleVariant = fp.vehicleVariant,
          baseFare = fp.baseFare,
          baseDistance = fp.baseDistance,
          perExtraKmRate = fp.perExtraKmRate,
          nightShiftStart = fp.nightShiftStart,
          nightShiftEnd = fp.nightShiftEnd,
          nightShiftRate = fp.nightShiftRate
        }

updateFarePolicy :: RegToken.RegistrationToken -> Id DFarePolicy.FarePolicy -> UpdateFarePolicyRequest -> FlowHandler UpdateFarePolicyResponse
updateFarePolicy _ fpId req = withFlowHandlerAPI $ do
  farePolicy <- SFarePolicy.findFarePolicyById fpId >>= fromMaybeM NoFarePolicy
  let updatedFarePolicy =
        farePolicy
          { SFarePolicy.baseFare = req.baseFare,
            SFarePolicy.baseDistance = req.baseDistance,
            SFarePolicy.perExtraKmRate = req.perExtraKmRate,
            SFarePolicy.nightShiftStart = req.nightShiftStart,
            SFarePolicy.nightShiftEnd = req.nightShiftEnd,
            SFarePolicy.nightShiftRate = req.nightShiftRate
          }
  SFarePolicy.updateFarePolicy updatedFarePolicy
  pure Success
