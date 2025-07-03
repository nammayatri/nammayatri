{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PickupInstructions
  ( postPickupinstructions,
    getPickupinstructionsClosest,
  )
where

import qualified API.Types.UI.PickupInstructions as API
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.PickupInstructions as DPI
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Distance (highPrecMetersToMeters)
import qualified Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PickupInstructions as QPI
import Tools.Error

postPickupinstructions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.PickupInstructionsReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPickupinstructions (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  logDebug $ "PickupInstructions: Received POST request - personId: " <> show personId.getId <> ", lat: " <> show req.lat <> ", lon: " <> show req.lon <> ", instruction: " <> show req.instruction

  -- Validate instruction length (150 char limit from DB schema)
  when (length req.instruction > 150) $
    throwError $ InvalidRequest "Pickup instruction cannot exceed 150 characters"

  -- Get all existing instructions for this person
  existingInstructions <- QPI.findByPersonId personId
  logDebug $ "PickupInstructions: Found " <> show (length existingInstructions) <> " existing instructions from DB for personId: " <> show personId.getId

  let newLocation = LatLong req.lat req.lon
      proximityThreshold = fromIntegral riderConfig.pickupInstructionsProximityMeters

  logDebug $ "PickupInstructions: Proximity threshold configured as: " <> show proximityThreshold <> " meters"

  -- Check if any existing instruction is within proximity
  let mbNearbyInstruction =
        List.find
          ( \existing ->
              let existingLocation = LatLong existing.lat existing.lon
                  distance = distanceBetweenInMeters newLocation existingLocation
                  distanceInMeters = highPrecMetersToMeters distance
               in distanceInMeters <= proximityThreshold
          )
          existingInstructions

  case mbNearbyInstruction of
    Just nearbyInstruction -> do
      let existingLocation = LatLong nearbyInstruction.lat nearbyInstruction.lon
          distance = distanceBetweenInMeters newLocation existingLocation
          distanceInMeters = highPrecMetersToMeters distance
      logDebug $ "PickupInstructions: Found nearby instruction within " <> show distanceInMeters <> " meters. Updating existing instruction at lat: " <> show nearbyInstruction.lat <> ", lon: " <> show nearbyInstruction.lon <> " with new instruction: " <> show req.instruction
      -- Update existing instruction using the NEW coordinates and instruction
      QPI.updateByPersonIdAndLocation req.lat req.lon req.instruction personId
    Nothing -> do
      logDebug $ "PickupInstructions: No nearby instruction found. Current count: " <> show (length existingInstructions) <> ", threshold: " <> show riderConfig.pickupInstructionsThreshold

      -- Check if we're at the limit
      if length existingInstructions >= riderConfig.pickupInstructionsThreshold
        then do
          -- We're at the limit, find the oldest instruction (by updatedAt) and replace it
          case List.sortBy (comparing (.updatedAt)) existingInstructions of
            [] ->
              -- This shouldn't happen since we just checked length above
              throwError $ InternalError "Expected existing instructions but found none"
            (oldestInstruction : _) -> do
              logDebug $ "PickupInstructions: At threshold limit. Deleting oldest instruction id: " <> show oldestInstruction.id.getId <> " at lat: " <> show oldestInstruction.lat <> ", lon: " <> show oldestInstruction.lon <> " and creating new instruction"
              -- Delete the oldest instruction by its ID
              QPI.deleteById oldestInstruction.id
              -- Create new pickup instruction
              pickupInstructionsId <- generateGUID
              now <- getCurrentTime
              let newInstruction =
                    DPI.PickupInstructions
                      { DPI.id = pickupInstructionsId,
                        DPI.personId = personId,
                        DPI.merchantId = merchantId,
                        DPI.merchantOperatingCityId = person.merchantOperatingCityId,
                        DPI.lat = req.lat,
                        DPI.lon = req.lon,
                        DPI.instruction = req.instruction,
                        DPI.createdAt = now,
                        DPI.updatedAt = now
                      }
              logDebug $ "PickupInstructions: Creating replacement instruction - id: " <> show pickupInstructionsId.getId <> ", lat: " <> show req.lat <> ", lon: " <> show req.lon <> ", instruction: " <> show req.instruction
              QPI.create newInstruction
        else do
          -- Create new pickup instruction
          pickupInstructionsId <- generateGUID
          now <- getCurrentTime
          let newInstruction =
                DPI.PickupInstructions
                  { DPI.id = pickupInstructionsId,
                    DPI.personId = personId,
                    DPI.merchantId = merchantId,
                    DPI.merchantOperatingCityId = person.merchantOperatingCityId,
                    DPI.lat = req.lat,
                    DPI.lon = req.lon,
                    DPI.instruction = req.instruction,
                    DPI.createdAt = now,
                    DPI.updatedAt = now
                  }
          logDebug $ "PickupInstructions: Creating new instruction - id: " <> show pickupInstructionsId.getId <> ", lat: " <> show req.lat <> ", lon: " <> show req.lon <> ", instruction: " <> show req.instruction
          QPI.create newInstruction

  pure Kernel.Types.APISuccess.Success

getPickupinstructionsClosest ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Double ->
    Kernel.Prelude.Maybe Kernel.Prelude.Double ->
    Environment.Flow API.ClosestPickupInstructionResp
  )
getPickupinstructionsClosest (mbPersonId, _) mbLat mbLon = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  lat <- mbLat & fromMaybeM (InvalidRequest "Missing required parameter: lat")
  lon <- mbLon & fromMaybeM (InvalidRequest "Missing required parameter: lon")

  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  let queryLocation = LatLong lat lon
      proximityThreshold = fromIntegral riderConfig.pickupInstructionsProximityMeters

  logDebug $ "PickupInstructions: GET closest request for personId: " <> show personId.getId <> ", lat: " <> show lat <> ", lon: " <> show lon <> ", proximityThreshold: " <> show proximityThreshold <> "m"

  -- Get all pickup instructions for this person
  pickupInstructions <- QPI.findByPersonId personId

  if null pickupInstructions
    then do
      logDebug "PickupInstructions: No instructions found for user"
      return $ API.ClosestPickupInstructionResp Nothing
    else do
      -- Find instructions within proximity threshold
      let instructionsWithinProximity =
            List.filter
              ( \instruction ->
                  let instructionLocation = LatLong instruction.lat instruction.lon
                      distance = distanceBetweenInMeters queryLocation instructionLocation
                      distanceInMeters = highPrecMetersToMeters distance
                   in distanceInMeters <= proximityThreshold
              )
              pickupInstructions

      case instructionsWithinProximity of
        [] -> do
          logDebug $ "PickupInstructions: No instructions found within proximity threshold of " <> show proximityThreshold <> "m"
          return $ API.ClosestPickupInstructionResp Nothing
        proximityInstructions -> do
          -- Among instructions within proximity, find the closest one
          let instructionsWithDistance =
                map
                  ( \instruction ->
                      let instructionLocation = LatLong instruction.lat instruction.lon
                          distance = distanceBetweenInMeters queryLocation instructionLocation
                          distanceInMeters = highPrecMetersToMeters distance
                       in (instruction, distanceInMeters)
                  )
                  proximityInstructions

              -- Sort by distance and get the closest one
              sortedInstructions = List.sortBy (comparing snd) instructionsWithDistance

          case sortedInstructions of
            [] -> do
              logDebug "PickupInstructions: No instructions in sorted list (unexpected)"
              return $ API.ClosestPickupInstructionResp Nothing
            ((closestInstruction, distanceToClosest) : _) -> do
              logDebug $ "PickupInstructions: Found closest instruction within proximity at distance " <> show distanceToClosest <> "m with text: " <> show closestInstruction.instruction
              return $ API.ClosestPickupInstructionResp (Just closestInstruction.instruction)
