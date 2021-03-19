{-# LANGUAGE OverloadedLabels #-}

module Product.FarePolicy where

import App.Types (FlowHandler)
import Beckn.Types.Core.Ack
import Beckn.Types.Error
import Beckn.Types.Id (Id (..))
import qualified Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common (fromMaybeM400, fromMaybeMWithInfo400, withFlowHandler)
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

listFarePolicies :: RegToken.RegistrationToken -> FlowHandler ListFarePolicyResponse
listFarePolicies RegToken.RegistrationToken {_EntityId} = withFlowHandler $ do
  person <- SPerson.findPersonById (Id _EntityId)
  orgId <- person ^. #_organizationId & fromMaybeMWithInfo400 PersonInvalidState "_organizationId is null."
  farePolicies <- SFarePolicy.findFarePoliciesByOrgId (Id orgId)
  pure $ ListFarePolicyResponse $ toResponse <$> farePolicies
  where
    toResponse fp =
      FarePolicyResponse
        { id = fp ^. #_id,
          vehicleVariant = fp ^. #_vehicleVariant,
          baseFare = fp ^. #_baseFare,
          baseDistance = fp ^. #_baseDistance,
          perExtraKmRate = fp ^. #_perExtraKmRate,
          nightShiftStart = fp ^. #_nightShiftStart,
          nightShiftEnd = fp ^. #_nightShiftEnd,
          nightShiftRate = fp ^. #_nightShiftRate
        }

updateFarePolicy :: RegToken.RegistrationToken -> Id DFarePolicy.FarePolicy -> UpdateFarePolicyRequest -> FlowHandler UpdateFarePolicyResponse
updateFarePolicy _ fpId req = withFlowHandler $ do
  farePolicy <- SFarePolicy.findFarePolicyById fpId >>= fromMaybeM400 FarePolicyNotFound
  let updatedFarePolicy =
        farePolicy
          { SFarePolicy._baseFare = req ^. #baseFare,
            SFarePolicy._baseDistance = req ^. #baseDistance,
            SFarePolicy._perExtraKmRate = req ^. #perExtraKmRate,
            SFarePolicy._nightShiftStart = req ^. #nightShiftStart,
            SFarePolicy._nightShiftEnd = req ^. #nightShiftEnd,
            SFarePolicy._nightShiftRate = req ^. #nightShiftRate
          }
  _ <- SFarePolicy.updateFarePolicy updatedFarePolicy
  pure $ Ack {_status = "ACK"}
