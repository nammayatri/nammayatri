{-# LANGUAGE OverloadedLabels #-}

module Product.FarePolicy where

import App.Types (FlowHandler)
import Beckn.Types.ID (ID (..))
import Beckn.Utils.Common (fromMaybeM400, withFlowHandler)
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy as S
import Types.API.FarePolicy
  ( FarePolicyResponse (..),
    ListFarePolicyResponse (ListFarePolicyResponse),
    UpdateFarePolicyRequest,
  )
import qualified Types.Domain.FarePolicy as D
import qualified Types.Storage.FarePolicy as S

listFarePolicies :: Text -> FlowHandler ListFarePolicyResponse
listFarePolicies orgId = withFlowHandler $ do
  farePolicies <- S.findFarePoliciesByOrgId (ID orgId)
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

updateFarePolicy :: Text -> ID D.FarePolicy -> UpdateFarePolicyRequest -> FlowHandler ()
updateFarePolicy _orgId fpId req = withFlowHandler $ do
  farePolicy <- S.findFarePolicyById fpId >>= fromMaybeM400 "FARE_POLICY_NOT_FOUND"
  let updatedFarePolicy =
        farePolicy
          { S._baseFare = req ^. #baseFare,
            S._baseDistance = req ^. #baseDistance,
            S._perExtraKmRate = req ^. #perExtraKmRate,
            S._nightShiftStart = req ^. #nightShiftStart,
            S._nightShiftEnd = req ^. #nightShiftEnd,
            S._nightShiftRate = req ^. #nightShiftRate
          }
  _ <- S.updateFarePolicy updatedFarePolicy
  pure ()
