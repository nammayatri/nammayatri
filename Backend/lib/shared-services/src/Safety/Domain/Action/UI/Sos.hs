{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Safety.Domain.Action.UI.Sos where

import qualified IssueManagement.Domain.Types.MediaFile as DMF
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Beam.Functions as B
import qualified Safety.Domain.Types.Common as Common
import qualified Safety.Domain.Types.SafetySettings as DSafetySettings
import qualified Safety.Domain.Types.Sos as DSos
import Safety.Storage.BeamFlow
import qualified Safety.Storage.CachedQueries.Sos as CQSos
import qualified Safety.Storage.Queries.SafetySettings as QSafetySettings
import qualified Safety.Storage.Queries.Sos as QSos

-- | Update SOS entityType from NonRide to Ride and update rideId
-- Validates that the SOS exists and has entityType "NonRide" before updating
updateSosFromNonRideToRide ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id DSos.Sos ->
  Id Common.Ride ->
  m ()
updateSosFromNonRideToRide sosId newRideId = do
  sos <- QSos.findById sosId >>= fromMaybeM (InvalidRequest $ "SOS not found: " <> sosId.getId)

  unless (sos.entityType == Just DSos.NonRide) $
    throwError $ InvalidRequest $ "SOS entityType is not NonRide. Current entityType: " <> show sos.entityType

  let updatedSos =
        sos{entityType = Just DSos.Ride,
            rideId = Just newRideId
           }

  QSos.updateByPrimaryKey updatedSos

-- | Create a new SOS record
createSos ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  DSos.Sos ->
  m (Id DSos.Sos)
createSos sos = do
  QSos.create sos
  return sos.id

-- | Update SOS status
updateSosStatus ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  DSos.SosStatus ->
  Id DSos.Sos ->
  m ()
updateSosStatus status sosId = QSos.updateStatus status sosId

-- | Update SOS state
updateSosState ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Maybe DSos.SosState ->
  Id DSos.Sos ->
  m ()
updateSosState sosState sosId = QSos.updateState sosState sosId

-- | Update SOS media files
updateSosMediaFiles ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  [Id DMF.MediaFile] ->
  Id DSos.Sos ->
  m ()
updateSosMediaFiles mediaFiles sosId = QSos.updateMediaFiles mediaFiles sosId

-- | Update SOS tracking expiration time
updateSosTrackingExpiresAt ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Maybe UTCTime ->
  Id DSos.Sos ->
  m ()
updateSosTrackingExpiresAt trackingExpiresAt sosId = QSos.updateTrackingExpiresAt trackingExpiresAt sosId

-- | Find SOS by ID
findSosById ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id DSos.Sos ->
  m (Maybe DSos.Sos)
findSosById sosId = QSos.findById sosId

-- | Find SOS by ticket ID
findSosByTicketId ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Maybe Text ->
  m (Maybe DSos.Sos)
findSosByTicketId ticketId = QSos.findByTicketId ticketId

-- | Find SOS by person ID
findSosByPersonId ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id Common.Person ->
  m [DSos.Sos]
findSosByPersonId personId = QSos.findByPersonId personId

-- | Update mock safety drill status
updateMockSafetyDrillStatus ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Maybe Bool ->
  Id Common.Person ->
  m ()
updateMockSafetyDrillStatus hasCompletedMockSafetyDrill personId = QSafetySettings.updateMockSafetyDrillStatus hasCompletedMockSafetyDrill personId

-- | Result type for markSosAsSafe operation
data MarkSosAsSafeResult = MarkSosAsSafeResult
  { updatedSos :: DSos.Sos,
    safetySettings :: DSafetySettings.SafetySettings,
    wasLiveTracking :: Bool,
    shouldStopTracking :: Bool,
    shouldMarkAsResolved :: Bool,
    shouldNotifyContacts :: Bool,
    notificationKey :: Text,
    isRideBased :: Bool,
    rideId :: Maybe (Id Common.Ride)
  }
  deriving (Generic)

-- | Result type for createRideBasedSos operation
data CreateSosResult = CreateSosResult
  { sosId :: Id DSos.Sos,
    wasReactivated :: Bool,
    sosDetails :: DSos.Sos
  }
  deriving (Generic, Show)

-- | Helper function to check if SOS is ride-based
isRideBasedSos :: Maybe DSos.SosEntityType -> Bool
isRideBasedSos (Just DSos.Ride) = True
isRideBasedSos Nothing = True
isRideBasedSos _ = False

-- | Core logic for marking SOS as safe
-- Handles SOS DB operations, safetySettings DB operations, and Redis caching
markSosAsSafe ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id DSos.Sos ->
  Id Common.Person ->
  Maybe Bool -> -- isEndLiveTracking
  Maybe Bool -> -- isRideEnded
  m MarkSosAsSafeResult
markSosAsSafe sosId personId mbIsEndLiveTracking mbIsRideEnded = do
  -- Fetch SOS details (shared-services DB)
  sosDetails <- B.runInReplica $ QSos.findById sosId >>= fromMaybeM (InvalidRequest $ "SOS not found: " <> sosId.getId)

  -- Validate SOS status
  when (sosDetails.status == DSos.Resolved) $
    throwError $ InvalidRequest "Sos already resolved."

  -- Fetch safetySettings (shared-services DB)
  mbSafetySettings <- QSafetySettings.findByPersonId personId
  safetySettings <- case mbSafetySettings of
    Just ss -> return ss
    Nothing -> throwError $ InvalidRequest $ "SafetySettings not found for personId: " <> personId.getId

  let wasLiveTracking = sosDetails.sosState == Just DSos.LiveTracking
      shouldStopTracking = fromMaybe True mbIsEndLiveTracking
      isRideBased = isRideBasedSos sosDetails.entityType
      shouldMarkAsResolved = isRideBased || (mbIsEndLiveTracking == Just True)

  -- Update SOS status if needed (shared-services DB)
  when shouldMarkAsResolved $ do
    void $ updateSosStatus DSos.Resolved sosId

  -- Update SOS state for non-ride SOS transitioning to LiveTracking (shared-services DB)
  when (sosDetails.entityType == Just DSos.NonRide && sosDetails.sosState == Just DSos.SosActive && mbIsEndLiveTracking == Just False) $ do
    void $ updateSosState (Just DSos.LiveTracking) sosId

  -- Fetch updated SOS details from DB after all updates
  updatedSosDetails <- B.runInReplica $ QSos.findById sosId >>= fromMaybeM (InvalidRequest $ "Failed to fetch updated SOS: " <> sosId.getId)

  -- Cache SOS by rideId if ride-based and resolved (Redis - shared-services)
  when (isRideBased && shouldMarkAsResolved) $ do
    rideId <- updatedSosDetails.rideId & fromMaybeM (InvalidRequest "Ride ID not found")
    CQSos.cacheSosIdByRideId rideId updatedSosDetails

  -- Determine notification logic
  let shouldNotifyContacts =
        if isRideBased
          then safetySettings.notifySosWithEmergencyContacts && mbIsRideEnded /= Just True
          else True
      notificationKey =
        if isRideBased
          then "SOS_RESOLVED_SAFE"
          else
            if shouldStopTracking && wasLiveTracking
              then "LIVE_TRACKING_STOPPED"
              else "SOS_RESOLVED_SAFE"

  return $
    MarkSosAsSafeResult
      { updatedSos = updatedSosDetails,
        safetySettings = safetySettings,
        wasLiveTracking = wasLiveTracking,
        shouldStopTracking = shouldStopTracking,
        shouldMarkAsResolved = shouldMarkAsResolved,
        shouldNotifyContacts = shouldNotifyContacts,
        notificationKey = notificationKey,
        isRideBased = isRideBased,
        rideId = updatedSosDetails.rideId
      }

-- | Core logic for creating/updating ride-based SOS
-- Handles SOS DB operations and Redis caching
createRideBasedSos ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id Common.Person ->
  Id Common.Ride ->
  DSos.SosType ->
  Maybe Text -> -- ticketId
  m CreateSosResult
createRideBasedSos personId rideId flow ticketId = do
  -- Check if SOS exists for rideId (using cached query)
  mbExistingSos <- CQSos.findByRideId rideId

  case mbExistingSos of
    Just existingSos -> do
      -- Update existing SOS status to Pending
      void $ updateSosStatus DSos.Pending existingSos.id

      -- Fetch updated SOS from DB after update
      updatedSos <- B.runInReplica $ QSos.findById existingSos.id >>= fromMaybeM (InvalidRequest $ "Failed to fetch updated SOS: " <> existingSos.id.getId)

      -- Cache updated SOS
      CQSos.cacheSosIdByRideId rideId updatedSos

      return $
        CreateSosResult
          { sosId = existingSos.id,
            wasReactivated = True,
            sosDetails = updatedSos
          }
    Nothing -> do
      -- Create new SOS
      now <- getCurrentTime
      pid <- generateGUID
      let newSos =
            DSos.Sos
              { id = pid,
                personId = personId,
                status = DSos.Pending,
                flow = flow,
                rideId = Just rideId,
                ticketId = ticketId,
                mediaFiles = [],
                merchantId = Nothing,
                merchantOperatingCityId = Nothing,
                trackingExpiresAt = Nothing,
                entityType = Just DSos.Ride,
                sosState = Just DSos.SosActive,
                createdAt = now,
                updatedAt = now
              }

      void $ createSos newSos

      -- Cache new SOS
      CQSos.cacheSosIdByRideId rideId newSos

      return $
        CreateSosResult
          { sosId = newSos.id,
            wasReactivated = False,
            sosDetails = newSos
          }

-- | Core logic for creating non-ride SOS
-- Handles SOS DB operations
createNonRideSos ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Id Common.Person ->
  Maybe (Id Common.Merchant) ->
  Maybe (Id Common.MerchantOperatingCity) ->
  Maybe UTCTime -> -- trackingExpiresAt
  DSos.SosType ->
  m DSos.Sos
createNonRideSos personId mbMerchantId mbMerchantOperatingCityId mbTrackingExpiresAt flow = do
  now <- getCurrentTime
  pid <- generateGUID
  let newSos =
        DSos.Sos
          { id = pid,
            personId = personId,
            status = DSos.Pending,
            flow = flow,
            rideId = Nothing,
            ticketId = Nothing,
            mediaFiles = [],
            merchantId = mbMerchantId,
            merchantOperatingCityId = mbMerchantOperatingCityId,
            trackingExpiresAt = mbTrackingExpiresAt,
            entityType = Just DSos.NonRide,
            sosState = Just DSos.SosActive,
            createdAt = now,
            updatedAt = now
          }

  void $ createSos newSos
  return newSos

-- | Update SOS status and handle caching for ride-based SOS
updateSosStatusWithCache ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id DSos.Sos ->
  DSos.SosStatus ->
  Id Common.Person -> -- for validation
  m DSos.Sos
updateSosStatusWithCache sosId status personId = do
  -- Fetch SOS (validate personId)
  sos <- B.runInReplica $ QSos.findById sosId >>= fromMaybeM (InvalidRequest $ "SOS not found: " <> sosId.getId)

  unless (sos.personId == personId) $
    throwError $ InvalidRequest "SOS does not belong to the specified person"

  -- Update SOS status
  void $ updateSosStatus status sosId

  -- Fetch updated SOS from DB after update
  updatedSos <- B.runInReplica $ QSos.findById sosId >>= fromMaybeM (InvalidRequest $ "Failed to fetch updated SOS: " <> sosId.getId)

  -- If ride-based: cache updated SOS
  when (isRideBasedSos updatedSos.entityType) $ do
    rideId <- updatedSos.rideId & fromMaybeM (InvalidRequest "Ride ID not found")
    CQSos.cacheSosIdByRideId rideId updatedSos

  return updatedSos

-- | Core logic for starting SOS tracking (non-ride)
-- Handles SOS DB operations
startSosTracking ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Maybe (Id DSos.Sos) ->
  Id Common.Person ->
  Maybe (Id Common.Merchant) ->
  Maybe (Id Common.MerchantOperatingCity) ->
  UTCTime -> -- trackingExpiresAt
  DSos.SosType ->
  m (Id DSos.Sos)
startSosTracking mbSosId personId mbMerchantId mbMerchantOperatingCityId trackingExpiresAt flow = do
  case mbSosId of
    Just sosId -> do
      -- Update existing SOS tracking expiration
      void $ updateSosTrackingExpiresAt (Just trackingExpiresAt) sosId
      return sosId
    Nothing -> do
      -- Create new SOS for tracking
      newSos <- createNonRideSos personId mbMerchantId mbMerchantOperatingCityId (Just trackingExpiresAt) flow
      return newSos.id

-- | Update SOS state and tracking expiration
-- Handles SOS DB operations and validation
updateSosStateWithTracking ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  DSos.Sos ->
  Id Common.Person ->
  DSos.SosState ->
  Maybe UTCTime -> -- mbTrackingExpiresAt
  m DSos.Sos
updateSosStateWithTracking sos personId newState mbTrackingExpiresAt = do
  -- Validate personId, entityType, and status

  unless (sos.personId == personId) $
    throwError $ InvalidRequest "SOS does not belong to the specified person"

  unless (sos.entityType == Just DSos.NonRide) $
    throwError $ InvalidRequest "This function is only for non-ride SOS"

  unless (sos.status == DSos.Pending) $
    throwError $ InvalidRequest "Can only update state for pending SOS"

  -- Update SOS state
  void $ updateSosState (Just newState) sos.id

  -- Update tracking expiration if provided
  whenJust mbTrackingExpiresAt $ \trackingExpiresAt ->
    void $ updateSosTrackingExpiresAt (Just trackingExpiresAt) sos.id

  -- Fetch updated SOS from DB after all updates
  updatedSos <- B.runInReplica $ QSos.findById sos.id >>= fromMaybeM (InvalidRequest $ "Failed to fetch updated SOS: " <> sos.id.getId)

  return updatedSos

-- | Update SOS state with automatic expiry calculation based on state transition
-- Handles SOS DB operations, validation, and expiry time calculation
updateSosStateWithAutoExpiry ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    MonadTime m
  ) =>
  Id Common.Person ->
  DSos.Sos ->
  DSos.SosState ->
  m DSos.Sos
updateSosStateWithAutoExpiry personId sosData newState = do
  -- Calculate expiry time based on state transition
  let oldState = sosData.sosState
  mbExpiryTime <- case (oldState, newState) of
    (Just DSos.LiveTracking, DSos.SosActive) -> do
      -- When transitioning from LiveTracking to SosActive, extend expiry by 8 hours
      now <- getCurrentTime
      let eightHoursInSeconds :: Int = 8 * 60 * 60
      pure $ Just $ addUTCTime (fromIntegral eightHoursInSeconds) now
    _ ->
      -- For other transitions, don't set expiry (use existing or None)
      pure Nothing

  -- Call the base function with calculated expiry
  updateSosStateWithTracking sosData personId newState mbExpiryTime
