{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Safety.Domain.Action.UI.Sos where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified EulerHS.Language as EL
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.BeamFlow as IssueBeamFlow
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.SOS as PoliceSOS
import qualified Kernel.External.SOS.Interface.Types as SOSInterface
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Safety.API.UI.Sos (AddSosVideoRes (..), SOSVideoUploadReq (..))
import qualified Safety.API.Types.UI.Sos as API
import qualified Safety.Domain.Types.Common as Common
import qualified Safety.Domain.Types.SafetySettings as DSafetySettings
import qualified Safety.Domain.Types.Sos as DSos
import Safety.Storage.BeamFlow
import qualified Safety.Storage.CachedQueries.Sos as CQSos
import qualified Safety.Storage.CachedQueries.SosLocation as CQSosLocation
import qualified Safety.Storage.Queries.PersonDefaultEmergencyNumberExtra as SafetyQPDEN
import qualified Safety.Storage.Queries.SafetySettings as QSafetySettings
import qualified Safety.Storage.Queries.SafetySettingsExtra as QSafetyExtra
import qualified Safety.Storage.Queries.Sos as QSos
import Safety.Tools.Error

-- | Update SOS entityType from NonRide to Ride and update rideId
-- Validates that the SOS exists and has entityType "NonRide" before updating
updateSosFromNonRideToRide ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  DSos.Sos ->
  Id Common.Ride ->
  m ()
updateSosFromNonRideToRide sos newRideId = do
  unless (sos.entityType == Just DSos.NonRide) $
    throwError $ InvalidRequest $ "SOS entityType is not NonRide. Current entityType: " <> show sos.entityType

  let updatedSos =
        sos{entityType = Just DSos.Ride,
            rideId = Just newRideId,
            sosState = Just DSos.SosActive
           }

  QSos.updateByPrimaryKey updatedSos
  CQSos.cacheSosIdByRideId newRideId updatedSos

-- | Update ticketId on an existing SOS (e.g. after creating Kapture ticket when converting non-ride SOS to ride).
-- Updates DB and cache when SOS has a rideId.
updateSosTicketId ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  DSos.Sos ->
  Maybe Text ->
  m ()
updateSosTicketId sos mbTicketId = do
  let updatedSos = sos {DSos.ticketId = mbTicketId}
  QSos.updateByPrimaryKey updatedSos
  whenJust sos.rideId $ \rid -> CQSos.cacheSosIdByRideId rid updatedSos

-- | Create a new SOS record
createSos ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  DSos.Sos ->
  m (Id DSos.Sos)
createSos sos = do
  logDebug $ "createSos: before QSos.create, sosId=" <> sos.id.getId <> ", personId=" <> sos.personId.getId <> ", rideId=" <> maybe "nothing" (.getId) sos.rideId
  QSos.create sos
  logDebug $ "createSos: after QSos.create, sosId=" <> sos.id.getId
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
    CoreMetrics m
  ) =>
  Id DSos.Sos ->
  m (Maybe DSos.Sos)
findSosById sosId = QSos.findById sosId

-- | Find SOS by ticket ID
findSosByTicketId ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Maybe Text ->
  m (Maybe DSos.Sos)
findSosByTicketId ticketId = QSos.findByTicketId ticketId

-- | Find SOS by person ID
findSosByPersonId ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Id Common.Person ->
  m [DSos.Sos]
findSosByPersonId personId = QSos.findByPersonId personId

-- | Find active SOS for a person (status is Pending).
-- Returns the most recent one by createdAt if multiple exist.
findActiveSosByPersonId ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id Common.Person ->
  m (Maybe DSos.Sos)
findActiveSosByPersonId personId = do
  listToMaybe <$> QSos.findActiveByPersonId (Just 1) Nothing personId [DSos.Pending]

findSosByRideId ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id Common.Ride ->
  m (Maybe DSos.Sos)
findSosByRideId rideId = QSos.findByRideId (Just rideId)

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
  sosDetails <- B.runInReplica $ QSos.findById sosId >>= fromMaybeM (SosNotFound sosId.getId)
  unless (sosDetails.personId == personId) $
    throwError $ SosNotAuthorized sosId.getId
  -- Validate SOS status
  when (sosDetails.status == DSos.Resolved) $
    throwError $ SosAlreadyResolved sosId.getId

  -- Fetch safetySettings (shared-services DB)
  mbSafetySettings <- QSafetySettings.findByPersonId personId
  safetySettings <- case mbSafetySettings of
    Just ss -> return ss
    Nothing -> throwError $ SosSafetySettingsNotFound personId.getId

  let wasLiveTracking = sosDetails.sosState == Just DSos.LiveTracking
      shouldStopTracking = fromMaybe True mbIsEndLiveTracking
      isRideBased = isRideBasedSos sosDetails.entityType
      shouldMarkAsResolved = isRideBased || (mbIsEndLiveTracking == Just True)
      shouldTransitionToLiveTracking = sosDetails.entityType == Just DSos.NonRide && sosDetails.sosState == Just DSos.SosActive && mbIsEndLiveTracking == Just False

  when (not shouldMarkAsResolved && not shouldTransitionToLiveTracking) $
    throwError $ SosNoActionRequired sosId.getId

  now <- getCurrentTime
  let updatedSosDetails =
        sosDetails
          { DSos.status = if shouldMarkAsResolved then DSos.Resolved else sosDetails.status,
            DSos.sosState = if shouldTransitionToLiveTracking then Just DSos.LiveTracking else sosDetails.sosState,
            DSos.updatedAt = now
          }
  void $ QSos.updateByPrimaryKey updatedSosDetails

  -- Cache SOS by rideId if ride-based and resolved (Redis - shared-services)
  when (isRideBased && shouldMarkAsResolved) $ do
    -- Legacy safety_sos rows may have entityType = Nothing but rideId = Nothing.
    -- Don't fail on caching in that case.
    whenJust updatedSosDetails.rideId $ \rideId ->
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
  Id Common.MerchantOperatingCity ->
  Id Common.Merchant ->
  DSos.SosType ->
  Maybe DSos.Sos ->
  Maybe Text -> -- ticketId
  Maybe Text -> -- externalReferenceId
  m CreateSosResult
createRideBasedSos personId rideId merchantOperatingCityId merchantId flow mbExistingSos ticketId mbExternalReferenceId = do
  -- Reuse active SOS only if it belongs to the same ride; resolve stale ones from other rides
  mbActive <- findActiveSosByPersonId personId
  mbReuse <- case mbActive of
    Just active
      | active.rideId == Just rideId -> pure $ Just active
    Just stale -> do
      void $ updateSosStatus DSos.Resolved stale.id
      whenJust stale.rideId CQSos.clearCache
      pure Nothing
    Nothing -> pure Nothing
  case mbReuse of
    Just active ->
      return $
        CreateSosResult
          { sosId = active.id,
            wasReactivated = True,
            sosDetails = active
          }
    Nothing -> case mbExistingSos of
      Just existingSos -> do
        now <- getCurrentTime
        let updatedSos = existingSos {DSos.status = DSos.Pending, DSos.updatedAt = now}
        void $ QSos.updateByPrimaryKey updatedSos

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
        let eightHoursInSeconds :: Int = 8 * 60 * 60
        let trackingExpiresAt = addUTCTime (fromIntegral eightHoursInSeconds) now
        let newSos =
              DSos.Sos
                { id = pid,
                  personId = personId,
                  status = DSos.Pending,
                  flow = flow,
                  rideId = Just rideId,
                  ticketId = ticketId,
                  mediaFiles = [],
                  merchantId = Just merchantId,
                  merchantOperatingCityId = Just merchantOperatingCityId,
                  trackingExpiresAt = Just trackingExpiresAt,
                  entityType = Just DSos.Ride,
                  sosState = Just DSos.SosActive,
                  createdAt = now,
                  updatedAt = now,
                  externalReferenceId = mbExternalReferenceId,
                  externalReferenceStatus = Nothing,
                  externalStatusHistory = Nothing
                }

        logDebug $ "createRideBasedSos: about to createSos, sosId=" <> getId pid <> ", rideId=" <> rideId.getId
        void $ createSos newSos
        logDebug $ "createRideBasedSos: createSos returned, sosId=" <> getId pid

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
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id Common.Person ->
  Maybe (Id Common.Merchant) ->
  Maybe (Id Common.MerchantOperatingCity) ->
  Maybe UTCTime -> -- trackingExpiresAt
  Maybe Text -> -- externalReferenceId
  DSos.SosType ->
  DSos.SosState ->
  m DSos.Sos
createNonRideSos personId mbMerchantId mbMerchantOperatingCityId mbTrackingExpiresAt mbExternalReferenceId flow state = do
  -- Only one active SOS per person: reuse existing active if any
  mbActive <- findActiveSosByPersonId personId
  case mbActive of
    Just active ->
      if active.sosState /= Just state
        then do
          now <- getCurrentTime
          let updated = active {DSos.sosState = Just state, DSos.updatedAt = now}
          void $ QSos.updateByPrimaryKey updated
          return updated
        else return active
    Nothing -> do
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
                sosState = Just state,
                createdAt = now,
                updatedAt = now,
                externalReferenceId = mbExternalReferenceId,
                externalReferenceStatus = Nothing,
                externalStatusHistory = Nothing
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

  now <- getCurrentTime
  let updatedSos = sos {DSos.status = status, DSos.updatedAt = now}
  void $ QSos.updateByPrimaryKey updatedSos

  -- If ride-based: cache updated SOS
  when (isRideBasedSos updatedSos.entityType) $
    whenJust updatedSos.rideId $ \rideId ->
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
  Maybe Text -> -- externalReferenceId
  DSos.SosType ->
  m (Id DSos.Sos)
startSosTracking mbSosId personId mbMerchantId mbMerchantOperatingCityId trackingExpiresAt mbExternalReferenceId flow = do
  case mbSosId of
    Just sosId -> do
      -- Update existing SOS tracking expiration
      sos <- QSos.findById sosId >>= fromMaybeM (InvalidRequest $ "SOS not found: " <> sosId.getId)
      unless (sos.personId == personId) $
        throwError $ InvalidRequest "SOS does not belong to the specified person"
      void $ updateSosTrackingExpiresAt (Just trackingExpiresAt) sosId
      return sosId
    Nothing -> do
      -- Create new SOS for tracking
      newSos <- createNonRideSos personId mbMerchantId mbMerchantOperatingCityId (Just trackingExpiresAt) mbExternalReferenceId flow DSos.LiveTracking
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
  Maybe Text -> -- mbTicketId
  m DSos.Sos
updateSosStateWithTracking sos personId newState mbTrackingExpiresAt mbTicketId = do
  -- Validate personId, entityType, and status

  unless (sos.personId == personId) $
    throwError $ InvalidRequest "SOS does not belong to the specified person"

  unless (sos.entityType == Just DSos.NonRide) $
    throwError $ InvalidRequest "This function is only for non-ride SOS"

  unless (sos.status == DSos.Pending) $
    throwError $ InvalidRequest "Can only update state for pending SOS"

  now <- getCurrentTime
  let updatedSos =
        sos{DSos.sosState = Just newState,
            DSos.trackingExpiresAt = mbTrackingExpiresAt <|> sos.trackingExpiresAt,
            DSos.ticketId = mbTicketId <|> sos.ticketId,
            DSos.updatedAt = now
           }
  void $ QSos.updateByPrimaryKey updatedSos

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
  Maybe Text -> -- mbTicketId
  m DSos.Sos
updateSosStateWithAutoExpiry personId sosData newState mbTicketId = do
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

  -- Call the base function with calculated expiry and ticketId
  updateSosStateWithTracking sosData personId newState mbExpiryTime mbTicketId

----------------------------------------------------------------------
-- UNIFIED SOS API — typeclasses, config, service handle, and handler
-- stubs. Added in Step 1 of the SOS unification plan. Steps 2–8 replace
-- each stub body with the real implementation.
-- See Backend/lib/shared-services/docs/SosAPIUnification.md for the full plan.
----------------------------------------------------------------------

-- | Extract a safety-domain personId from an auth tuple.
-- Instances cover rider's 2-tuple and driver's 3-tuple TokenAuth shapes.
class BuildSosCtx authToken m where
  extractSosPersonId :: authToken -> m (Id Common.Person)

-- Rider: (Id Person, Id Merchant)
instance Monad m => BuildSosCtx (Id a, Id b) m where
  extractSosPersonId (personId, _) = pure (cast personId)

-- Driver: (Id Person, Id Merchant, Id MerchantOpCity)
instance Monad m => BuildSosCtx (Id a, Id b, Id c) m where
  extractSosPersonId (personId, _, _) = pure (cast personId)

-- | Static per-platform feature flags (plus static rate-limit configs).
-- Per-request / per-merchantOpCity values like mediaFileSizeLimit live on
-- SosPersonData, not here. See §3 of the plan doc for the gate table.
data PlatformConfig = PlatformConfig
  { enableFollowRide :: Bool,
    enableShareRide :: Bool,
    enableMockDrill :: Bool,
    enableExternalSos :: Bool,
    enableIvr :: Bool,
    enablePoliceCall :: Bool,
    sosTrackingRateLimitOptions :: APIRateLimitOptions,
    erssStatusUpdateRateLimitOptions :: APIRateLimitOptions
  }
  deriving (Generic)

-- | Per-request person context. Built once by the app's getPersonData
-- callback; shared-services reads fields directly.
data SosPersonData = SosPersonData
  { personId :: Id Common.Person,
    personName :: Text,
    merchantId :: Id Common.Merchant,
    merchantOperatingCityId :: Id Common.MerchantOperatingCity,
    merchantShortId :: Text,
    personCityCode :: Text,
    personMobile :: Maybe Text,
    enableSupportForSafety :: Bool,
    kaptureDisposition :: Text,
    kaptureQueue :: Text,
    ticketClassification :: Ticket.Classification,
    personDisplayName :: Text,
    trackingUrlPattern :: Maybe Text,
    shareRideTrackingUrlPattern :: Text,
    dashboardMediaUrlPattern :: Maybe Text,
    mediaFileSizeLimit :: Integer,
    mediaFileUrlPattern :: Text,
    externalSOSConfig :: Maybe Common.ExternalSOSConfig,
    timeDiffFromUtc :: Seconds,
    safetySettingsDefaults :: Maybe QSafetyExtra.SafetySettingsPersonDefaults
  }
  deriving (Generic)

-- | Per-ride context. Populated by the app's getRideCtx callback.
-- The `ownerPersonId` field allows shared-services to validate ownership
-- directly without a separate callback.
data SosRideCtx = SosRideCtx
  { rideId :: Id Common.Ride,
    rideShortId :: ShortId Common.Ride,
    ownerPersonId :: Id Common.Person,
    rideEndTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    driverName :: Maybe Text,
    driverMobile :: Maybe Text,
    vehicleNumber :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleVariant :: Maybe Text,
    fromLat :: Double,
    fromLon :: Double,
    trackLink :: Text,
    rideInfo :: Maybe Ticket.RideInfo,
    rideCityCode :: Text
  }
  deriving (Generic)

-- | Minimal shared emergency-contact shape. `mobileCountryCode` + `mobileNumber`
-- are kept separate (not pre-concatenated) because downstream SMS/push plumbing
-- on rider/driver expects both fields individually.
data SosEmergencyContact = SosEmergencyContact
  { mobileCountryCode :: Text,
    mobileNumber :: Text,
    name :: Text,
    contactPersonId :: Maybe (Id Common.Person)
  }
  deriving (Generic, Show)

-- | Result of the external-SOS trigger callback.
data ExternalSosResult = ExternalSosResult
  { externalReferenceId :: Maybe Text,
    serviceConfig :: Maybe SOSInterface.SOSServiceConfig,
    apiCallSucceeded :: Maybe Bool
  }
  deriving (Generic)

-- | Parameters for building an SMS alert.
data SosAlertParams = SosAlertParams
  { userName :: Text,
    rideLink :: Text,
    rideEndTime :: Maybe Text,
    isRideEnded :: Bool
  }
  deriving (Generic)

-- | Service handle — injected per app via the HasSosHandle typeclass.
--
-- Notification design note: we consolidated the earlier `buildSosAlertSms` +
-- `sendSosNotification` pair into a single `sendSosNotification` that takes
-- `Maybe SosAlertParams`. The SMS is built (if needed) inside the callback
-- impl, so the SMS-template lookup is scoped to the notification fork — this
-- matches rider's existing pattern and fixes the driver `sosCreate` 500 on
-- missing SMS templates (§4 deliberate change #6).
data SosServiceHandle m = SosServiceHandle
  { getPlatformConfig :: PlatformConfig,
    getPersonData :: Id Common.Person -> m SosPersonData,
    getRideCtx :: Id Common.Ride -> m SosRideCtx,
    enableFollowRideForContacts :: Id Common.Person -> [SosEmergencyContact] -> m (),
    sendSosNotification ::
      SosPersonData ->
      Either Text (Text, Text) ->
      Notification.Category ->
      [(Text, Text)] ->
      Maybe SosAlertParams ->
      Bool ->
      [SosEmergencyContact] ->
      Maybe (Id DSos.Sos) ->
      m (),
    callKaptureCreateTicket ::
      Id Common.Merchant -> Id Common.MerchantOperatingCity -> Ticket.CreateTicketReq -> m (Maybe Text),
    callKaptureUpdateTicket ::
      Id Common.Merchant -> Id Common.MerchantOperatingCity -> Ticket.UpdateTicketReq -> m (),
    triggerExternalSos ::
      SosPersonData -> API.SosReq -> Maybe SosRideCtx -> m ExternalSosResult,
    sendExternalSosTrace :: DSos.Sos -> API.SosLocationUpdateReq -> m (),
    uploadExternalSosMedia :: SosPersonData -> Id DSos.Sos -> Text -> FilePath -> Text -> m (),
    registerSosWithLts ::
      Id DSos.Sos ->
      SosPersonData ->
      Maybe Text ->
      Maybe SOSInterface.SOSServiceConfig ->
      m (),
    processIvrOutcome ::
      Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m APISuccess,
    triggerPoliceCall :: SosPersonData -> Id Common.Ride -> m APISuccess,
    -- | Build the tracking URL shown to emergency contacts. Each app extracts
    -- the identifier it needs from SosRideCtx:
    --   Rider: rideCtx.rideId (UUID, appended as-is to trackingShortUrlPattern)
    --   Driver: rideCtx.rideShortId (short-code, appended to transporterConfig pattern)
    -- `Nothing` = non-ride SOS (rider uses sosTrackingLink + sosId; driver doesn't
    -- support non-ride flows so shouldn't be called with Nothing in practice).
    buildSosTrackingUrl :: SosPersonData -> Maybe SosRideCtx -> Id DSos.Sos -> Text,
    getSosRideDetails :: ShortId Common.Ride -> m API.RideDetailsForDriverRes
  }

-- | Typeclass provided by each app's Domain.Action.UI.Sos module.
class HasSosHandle r m | m -> r where
  getSosHandle :: m (SosServiceHandle m)

-- | Wrap a raw Kapture create HTTP call with `withTryCatch` + ticketId
-- extraction. Each app's callback invokes this, passing its own underlying
-- `Tools.Ticket.createTicket` action.
wrapKaptureCreateTicket ::
  (MonadFlow m, Log m) =>
  m Ticket.CreateTicketResp ->
  m (Maybe Text)
wrapKaptureCreateTicket action = do
  result <- withTryCatch "createTicket:sos" action
  pure $ case result of
    Right r -> Just r.ticketId
    Left _ -> Nothing

-- | Wrap a raw Kapture update HTTP call with a `fork` so the caller doesn't
-- wait on the Kapture round-trip. Preserves the original "updateTicket:sos"
-- fork label for log continuity.
wrapKaptureUpdateTicket ::
  (MonadFlow m) =>
  m Ticket.UpdateTicketResp ->
  m ()
wrapKaptureUpdateTicket action =
  fork "updateTicket:sos" $ void action

-- | Cached external-SOS trace state. Stored under @CQSos.mkExternalSOSTraceKey@.
-- Tuple: (lastTraceEpochSec, shouldCall, resolvedServiceConfig). The
-- @shouldCall@ flag is sticky for the cache TTL (3600s) so we don't re-resolve
-- the SOSServiceConfig on every location ping.
type ExternalSosTraceCache = (Integer, Bool, Maybe SOSInterface.SOSServiceConfig)

-- | Issue one SOS trace (location ping) to the configured external provider.
-- Pure plumbing — builds the request, dispatches via @PoliceSOS.sendSOSTrace@,
-- and throws on failure. No app-specific types involved.
callSosTraceApi ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  SOSInterface.SOSServiceConfig ->
  Text ->
  API.SosLocationUpdateReq ->
  Seconds ->
  m ()
callSosTraceApi trackingId specificConfig mobileNo req timeDiff = do
  now <- getCurrentTime
  let localNow = addUTCTime (secondsToNominalDiffTime timeDiff) now
  let dateTimeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localNow
      traceReq =
        SOSInterface.SOSTraceReq
          { SOSInterface.trackingId = trackingId,
            SOSInterface.mobileNo = mobileNo,
            SOSInterface.dateTime = dateTimeStr,
            SOSInterface.latitude = req.lat,
            SOSInterface.longitude = req.lon,
            SOSInterface.speed = Nothing,
            SOSInterface.gpsAccuracy = req.accuracy
          }
  traceRes <- PoliceSOS.sendSOSTrace specificConfig traceReq
  unless traceRes.success $
    throwError $ InternalError (fromMaybe "SOS Trace failed" traceRes.errorMessage)

-- | Generic external-SOS trace polling skeleton. The polling cadence,
-- person-mobile lookup, and service-config resolution are app-specific —
-- everything else (cache check, ttl, dispatch decision, trace dispatch)
-- lives here so each app's @sendExternalSosTrace@ callback shrinks to a
-- 3-line wiring of these three readers.
runExternalSosTrace ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  -- | Polling config reader: returns @(pollingIntervalSec, timeDiffFromUtc)@
  (Maybe (Id Common.MerchantOperatingCity) -> m (Int, Seconds)) ->
  -- | Person mobile number lookup (decrypted)
  (Id Common.Person -> m (Maybe Text)) ->
  -- | Service config resolver (returns Nothing if external SOS is not configured for this SOS)
  (DSos.Sos -> m (Maybe SOSInterface.SOSServiceConfig)) ->
  DSos.Sos ->
  API.SosLocationUpdateReq ->
  m ()
runExternalSosTrace getPollingConfig getPersonMobile resolveConfig sosDetails req = do
  let sosId = sosDetails.id
  (pollingIntervalSec, timeDiff) <- getPollingConfig sosDetails.merchantOperatingCityId
  cached :: Maybe ExternalSosTraceCache <- Redis.safeGet (CQSos.mkExternalSOSTraceKey sosId)
  now <- getCurrentTime
  let nowSec = round $ utcTimeToPOSIXSeconds now
      ttl = 3600 :: Int -- TODO: drive from per-config TTL once available (see govt requirement)
  case cached of
    Just (_, False, _) -> pure ()
    Just (_, True, Nothing) -> pure ()
    Just (lastTraceSec, True, Just specificConfig) ->
      when (nowSec - lastTraceSec >= fromIntegral pollingIntervalSec) $
        whenJust sosDetails.externalReferenceId $ \trackingId -> do
          mbMobile <- getPersonMobile sosDetails.personId
          whenJust mbMobile $ \mobileNo -> do
            callSosTraceApi trackingId specificConfig mobileNo req timeDiff
            Redis.setExp (CQSos.mkExternalSOSTraceKey sosId) (nowSec, True, Just specificConfig) ttl
    Nothing -> do
      mbConfig <- resolveConfig sosDetails
      let shouldCall = isJust mbConfig
      Redis.setExp (CQSos.mkExternalSOSTraceKey sosId) (nowSec, shouldCall, mbConfig) ttl
      whenJust mbConfig $ \specificConfig ->
        whenJust sosDetails.externalReferenceId $ \trackingId -> do
          mbMobile <- getPersonMobile sosDetails.personId
          whenJust mbMobile $ \mobileNo ->
            callSosTraceApi trackingId specificConfig mobileNo req timeDiff

----------------------------------------------------------------------
-- Pure helpers absorbed into shared-services (Step 1, per plan)
----------------------------------------------------------------------

-- | ERSS external-status history entry; stored JSON-encoded on the SOS row.
data ExternalStatusEntry = ExternalStatusEntry
  { status :: Text,
    idErss :: Text,
    lastUpdatedTime :: Maybe Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Build dashboard media URLs for Kapture tickets.
-- Parameters: pattern, maybe rideId, maybe sosId, merchantShortId, cityCode.
buildDashboardMediaUrls :: Maybe Text -> Maybe Text -> Maybe Text -> Text -> Text -> [Text]
buildDashboardMediaUrls mbPattern mbRideId mbSosId merchantShortIdText cityCode =
  case (mbPattern, mbRideId, mbSosId) of
    (Just patternS, Just rideIdText, _) ->
      [ patternS
          & T.replace "<RIDES_OR_SOS>" "rides"
          & T.replace "<ID>" rideIdText
          & T.replace "<RIDE_ID>" rideIdText
          & T.replace "<MERCHANT_SHORT_ID>" merchantShortIdText
          & T.replace "<CITY_CODE>" cityCode
      ]
    (Just patternS, Nothing, Just sosIdText) ->
      [ patternS
          & T.replace "<RIDES_OR_SOS>" "sos"
          & T.replace "<ID>" sosIdText
          & T.replace "<RIDE_ID>" sosIdText
          & T.replace "<MERCHANT_SHORT_ID>" merchantShortIdText
          & T.replace "<CITY_CODE>" cityCode
      ]
    _ -> []

-- | Kapture issue description — shared text, platform-branched on the caller's
-- ticket classification for the catch-all "SOS activated" case.
buildSosIssueDescription :: Ticket.Classification -> DSos.SosType -> Text
buildSosIssueDescription classification flow = case flow of
  DSos.Police -> "112 called"
  DSos.AudioRecording -> "Audio recording shared."
  DSos.CustomerCare -> "Customer care called."
  _ -> case classification of
    Ticket.DRIVER -> "SOS activated (driver)"
    _ -> "SOS activated"

-- | Fetch and decrypt a person's emergency contacts directly via the shared
-- PDEN queries. Replaces the old getEmergencyContacts callback on both apps —
-- both had this exact shape.
getSosEmergencyContactsShared ::
  ( BeamFlow m r,
    EncFlow m r
  ) =>
  Id Common.Person ->
  m [SosEmergencyContact]
getSosEmergencyContactsShared personId = do
  rows <- B.runInReplica $ SafetyQPDEN.findAllByPersonId personId
  decList <- mapM decrypt rows
  pure
    [ SosEmergencyContact
        { mobileCountryCode = pden.mobileCountryCode,
          mobileNumber = pden.mobileNumber,
          name = pden.name,
          contactPersonId = cast <$> pden.contactPersonId
        }
      | pden <- decList
    ]

-- | Build a synthetic Sos value from a cached mock-drill entity.
-- Used by sosGetDetails' fallback when the real SOS row is absent but a mock
-- is pending in Redis.
buildMockSos :: Id Common.Ride -> DSos.SosMockDrill -> UTCTime -> DSos.Sos
buildMockSos rideIdForMock mockSos now =
  DSos.Sos
    { flow = DSos.SafetyFlow,
      id = Id "mock-sos",
      personId = cast mockSos.personId,
      rideId = Just rideIdForMock,
      status = mockSos.status,
      ticketId = Nothing,
      mediaFiles = [],
      merchantId = Nothing,
      merchantOperatingCityId = Nothing,
      trackingExpiresAt = Nothing,
      sosState = Nothing,
      entityType = Nothing,
      externalReferenceId = Nothing,
      externalReferenceStatus = Nothing,
      externalStatusHistory = Nothing,
      createdAt = now,
      updatedAt = now
    }

----------------------------------------------------------------------
-- Handler stubs (17). Each is replaced with the real implementation in
-- the corresponding step (2–8). Until then, hitting any of these endpoints
-- returns a controlled 500 telling you exactly which step owns the impl.
----------------------------------------------------------------------

notYetImplemented :: (MonadFlow m) => Text -> m a
notYetImplemented name =
  throwError $ InternalError ("SOS handler '" <> name <> "' not yet implemented — see SosAPIUnification plan")

-- | Read the SOS row for a rider/driver ride.
-- Lightweight path by design: does NOT call h.getRideCtx (which would fetch
-- Ride+Booking from the app DB). Uses the shared SOS row as the single
-- source of truth for ownership (sos.personId). Preserves today's rider
-- mockSos fallback behind h.getPlatformConfig.enableMockDrill so driver
-- (enableMockDrill = False) keeps returning {sos = Nothing, externalSOSConfig = Nothing}.
sosGetDetails ::
  ( BeamFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  Id Common.Ride ->
  m API.SosDetailsRes
sosGetDetails authToken rideId = do
  personId <- extractSosPersonId authToken
  h <- getSosHandle
  personData <- h.getPersonData personId
  -- cached query with DB fallback
  mbSos <- CQSos.findByRideId rideId
  case mbSos of
    Just sos -> do
      unless (personId == sos.personId) $
        throwError $ InvalidRequest "PersonId not same"
      pure API.SosDetailsRes {sos = Just sos, externalSOSConfig = personData.externalSOSConfig}
    Nothing -> do
      mbMock <-
        if h.getPlatformConfig.enableMockDrill
          then Redis.safeGet (CQSos.mockSosKey personId)
          else pure Nothing
      case mbMock of
        Just (mockSos :: DSos.SosMockDrill) -> do
          now <- getCurrentTime
          pure
            API.SosDetailsRes
              { sos = Just (buildMockSos rideId mockSos now),
                externalSOSConfig = personData.externalSOSConfig
              }
        Nothing ->
          pure API.SosDetailsRes {sos = Nothing, externalSOSConfig = personData.externalSOSConfig}

-- | Exotel IVR webhook — gated by PlatformConfig.enableIvr (driver: False).
-- Logic itself is rider-specific (needs QCallStatus / QRide updates), so we
-- delegate to the app callback.
sosIvrOutcome ::
  ( MonadFlow m,
    HasSosHandle r m
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m APISuccess
sosIvrOutcome mbCallFrom mbCallSid mbCallStatus mbDigits = do
  h <- getSosHandle
  unless h.getPlatformConfig.enableIvr $
    throwError $ InvalidRequest "IVR is not supported on this platform"
  h.processIvrOutcome mbCallFrom mbCallSid mbCallStatus mbDigits

-- | The main SOS entry-point. Orchestrates:
--   * mock-drill clear
--   * safety-settings lookup (shared helper)
--   * ride context + ownership check (if rideId present)
--   * trigger dispatch (POLICE → external SOS; KAPTURE → ticket + SOS create)
--   * SOS row create/reactivate (shared helpers)
--   * LTS entity upsert (app callback, with external ref/config if any)
--   * emergency-contact notification fork (push + SMS fallback)
--
-- Platform flags PlatformConfig.{enableShareRide, enableFollowRide} gate the
-- share-ride flag flip and follow-ride enable; enableExternalSos is checked
-- inside h.triggerExternalSos by the driver stub.
sosCreate ::
  forall r m authToken.
  ( BeamFlow m r,
    IssueBeamFlow.BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  API.SosReq ->
  m API.SosRes
sosCreate authToken req = do
  personId <- extractSosPersonId authToken
  h <- getSosHandle
  personData <- h.getPersonData personId

  -- Clear any pending mock drill for this person (real SOS supersedes it)
  CQSos.clearMockDrill personId

  -- Load safety settings with fallback; drives the "notify contacts" gate below
  safetySettings <-
    QSafetyExtra.findSafetySettingsWithFallback
      personId
      (QSafetyExtra.getDefaultSafetySettings personId personData.safetySettingsDefaults)

  -- Ride context + ownership check (if rideId is on the request)
  mbRideCtx <- case req.rideId of
    Just rId -> do
      ctx <- h.getRideCtx rId
      unless (personId == ctx.ownerPersonId) $
        throwError $ InvalidRequest "Ride does not belong to this person"
      pure (Just ctx)
    Nothing -> pure Nothing

  -- Per-person active-SOS lookup. Done BEFORE any external API call so that
  -- we don't fire Trinity / Kapture only to discover the resulting refs have
  -- nowhere to land:
  --   * Same-ride re-trigger → short-circuit return (no externals, no DB writes).
  --   * Different-ride or non-ride leftover → resolve it before dispatch so the
  --     fresh caseId / ticketId land on a brand-new row.
  mbActiveSos <- findActiveSosByPersonId personId
  let mbSameRideActive = case (req.rideId, mbActiveSos) of
        (Just rId, Just active) | active.rideId == Just rId -> Just active
        _ -> Nothing

  case mbSameRideActive of
    Just active ->
      pure
        API.SosRes
          { sosId = active.id,
            externalSOSSuccess = if isJust active.externalReferenceId then Just True else Nothing,
            kaptureTicketId = active.ticketId
          }
    Nothing -> do
      -- Resolve any active SOS that doesn't match the current request
      -- (different-ride OR non-ride leftover). This prevents stale rows
      -- from absorbing a freshly-fetched Trinity caseId / Kapture ticket.
      whenJust mbActiveSos $ \active -> do
        void $ updateSosStatus DSos.Resolved active.id
        whenJust active.rideId CQSos.clearCache

      -- Look up any existing SOS for this ride — used by the ride-scoped
      -- reuse path (update existing Kapture ticket instead of opening a new one).
      mbExistingSos <- case req.rideId of
        Just rId -> CQSos.findByRideId rId
        Nothing -> pure Nothing
      let existingExternalRef = mbExistingSos >>= (.externalReferenceId)

      -- Dispatch triggers (POLICE + KAPTURE)
      (mbExternalReferenceId, mbTrigTicketId, mbSosIdFromTriggers, mbSosServiceConfig, externalApiCalledStatus) <-
        dispatchSosTriggersShared h personData req mbRideCtx mbExistingSos existingExternalRef

      -- Decide the final (sosId, ticketId, trackLink, rideEndTime) based on which
      -- path produced the SOS row
      (sosId, mbFinalTicketId, trackLink, mbRideEndTime) <- case (mbRideCtx, mbSosIdFromTriggers) of
        (Just rideCtx, Just sId) -> do
          let trackLink' = h.buildSosTrackingUrl personData (Just rideCtx) sId
              localEnd = addUTCTime (secondsToNominalDiffTime personData.timeDiffFromUtc) <$> rideCtx.rideEndTime
          pure (sId, mbTrigTicketId, trackLink', localEnd)
        (Just rideCtx, Nothing) -> do
          -- Ride exists but no trigger created the SOS (e.g. KAPTURE disabled).
          -- Create one directly via the shared helper.
          result <-
            createRideBasedSos
              personId
              rideCtx.rideId
              personData.merchantOperatingCityId
              personData.merchantId
              req.flow
              mbExistingSos
              (mbExistingSos >>= (.ticketId))
              mbExternalReferenceId
          let sId = result.sosId
              trackLink' = h.buildSosTrackingUrl personData (Just rideCtx) sId
              localEnd = addUTCTime (secondsToNominalDiffTime personData.timeDiffFromUtc) <$> rideCtx.rideEndTime
          pure (sId, result.sosDetails.ticketId, trackLink', localEnd)
        (Nothing, _) -> do
          -- Non-ride SOS flow: create, optionally open a Kapture ticket, stash
          -- location if provided on the request.
          now <- getCurrentTime
          let eightHoursInSeconds = 8 * 60 * 60 :: Int
          let trackingExpiresAt = addUTCTime (fromIntegral eightHoursInSeconds) now
          sosDetails <-
            createNonRideSos
              personId
              (Just personData.merchantId)
              (Just personData.merchantOperatingCityId)
              (Just trackingExpiresAt)
              mbExternalReferenceId
              req.flow
              DSos.SosActive
          whenJust req.customerLocation $ \location ->
            CQSosLocation.updateSosLocation sosDetails.id location Nothing (Just trackingExpiresAt)
          mbNonRideTicketId <-
            if personData.enableSupportForSafety
              && triggerListIncludesKapture req.triggerApiList
              && isNothing mbTrigTicketId
              && isNothing sosDetails.ticketId
              then createNonRideKaptureTicketShared h personData personId sosDetails.id req.flow
              else pure Nothing
          let finalTicketId = mbTrigTicketId <|> mbNonRideTicketId <|> sosDetails.ticketId
          whenJust finalTicketId $ \tId -> void $ updateSosTicketId sosDetails (Just tId)
          let finalTrackLink = h.buildSosTrackingUrl personData Nothing sosDetails.id
          pure (sosDetails.id, finalTicketId, finalTrackLink, Nothing)

      -- Register SOS with LTS (broadcaster config is built inside the callback
      -- from the externalReferenceId + service config we pass in).
      h.registerSosWithLts sosId personData mbExternalReferenceId mbSosServiceConfig

      -- Notify contacts in a fork so the HTTP response doesn't wait on SMS/push
      fork "postSosCreate:notifyEmergencyContacts" $ do
        let alertParams =
              SosAlertParams
                { userName = personData.personDisplayName,
                  rideLink = trackLink,
                  rideEndTime = T.pack . formatTime defaultTimeLocale "%e-%-m-%Y %-I:%M%P" <$> mbRideEndTime,
                  isRideEnded = fromMaybe False req.isRideEnded
                }
        case req.rideId of
          Just _ ->
            when (triggerShareRideAndNotify safetySettings) $ do
              emergencyContacts <- getSosEmergencyContactsShared personId
              when (req.isRideEnded /= Just True) $ do
                when h.getPlatformConfig.enableShareRide $
                  SafetyQPDEN.updateShareRideForAll personId True
                when h.getPlatformConfig.enableFollowRide $
                  h.enableFollowRideForContacts personId emergencyContacts
              let sosType =
                    if req.isRideEnded == Just True
                      then Notification.POST_RIDE_SOS_ALERT
                      else Notification.SOS_TRIGGERED
              when shouldNotifyPostRide $
                h.sendSosNotification
                  personData
                  (Left "SOS_ALERT")
                  sosType
                  [("userName", personData.personDisplayName)]
                  (Just alertParams)
                  True
                  emergencyContacts
                  Nothing
          Nothing -> do
            emergencyContacts <- getSosEmergencyContactsShared personId
            h.sendSosNotification
              personData
              (Left "SOS_ALERT")
              Notification.SOS_TRIGGERED
              [("userName", personData.personDisplayName)]
              (Just alertParams)
              True
              emergencyContacts
              (Just sosId)

      pure
        API.SosRes
          { sosId = sosId,
            externalSOSSuccess = externalApiCalledStatus,
            kaptureTicketId = mbFinalTicketId
          }
  where
    triggerShareRideAndNotify safetySettings =
      fromMaybe safetySettings.notifySosWithEmergencyContacts req.notifyAllContacts
        && req.flow == DSos.SafetyFlow

    -- Mirror rider's `shouldNotifyContacts`: for post-ride SOS, only notify
    -- when the client explicitly asks (`sendPNOnPostRideSOS = Just True`);
    -- for active-ride SOS, always notify.
    shouldNotifyPostRide = bool True (req.sendPNOnPostRideSOS == Just True) (req.isRideEnded == Just True)

-- | Trigger fold (POLICE + KAPTURE). Returns
-- (externalReferenceId, kaptureTicketId, sosId, sosServiceConfig, externalApiStatus).
-- Mirrors rider's dispatchSosTriggers — empty / Nothing triggerApiList
-- defaults to [KAPTURE].
dispatchSosTriggersShared ::
  ( BeamFlow m r,
    IssueBeamFlow.BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasSosHandle r m
  ) =>
  SosServiceHandle m ->
  SosPersonData ->
  API.SosReq ->
  Maybe SosRideCtx ->
  Maybe DSos.Sos ->
  Maybe Text ->
  m
    ( Maybe Text,
      Maybe Text,
      Maybe (Id DSos.Sos),
      Maybe SOSInterface.SOSServiceConfig,
      Maybe Bool
    )
dispatchSosTriggersShared h personData req mbRideCtx mbExistingSos existingExternalRef = do
  let triggers = case fromMaybe [] req.triggerApiList of
        [] -> [API.KAPTURE]
        xs -> xs
  Foldable.foldlM
    (processTrigger h personData req mbRideCtx mbExistingSos)
    (existingExternalRef, Nothing, Nothing, Nothing, Nothing)
    triggers

-- | Process one trigger (POLICE or KAPTURE) and fold its outputs into the
-- running tuple. Preserves rider's "reuse existing externalReferenceId" and
-- "only trigger police from FRONTEND" semantics.
processTrigger ::
  ( BeamFlow m r,
    IssueBeamFlow.BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasSosHandle r m
  ) =>
  SosServiceHandle m ->
  SosPersonData ->
  API.SosReq ->
  Maybe SosRideCtx ->
  Maybe DSos.Sos ->
  ( Maybe Text,
    Maybe Text,
    Maybe (Id DSos.Sos),
    Maybe SOSInterface.SOSServiceConfig,
    Maybe Bool
  ) ->
  API.TriggerApi ->
  m
    ( Maybe Text,
      Maybe Text,
      Maybe (Id DSos.Sos),
      Maybe SOSInterface.SOSServiceConfig,
      Maybe Bool
    )
processTrigger h personData req mbRideCtx mbExistingSos (externalReferenceId, kaptureTicketId, dbSosId, sosServiceConfig, externalApiStatus) trigger =
  case trigger of
    API.POLICE -> do
      (finalRef, finalSvc, finalStatus) <-
        case (externalReferenceId, personData.externalSOSConfig) of
          (Just existing, _) -> pure (Just existing, Nothing, Just True)
          (Nothing, Just sosConfig)
            | sosConfig.triggerSource == Common.FRONTEND -> do
                result <- h.triggerExternalSos personData req mbRideCtx
                pure (result.externalReferenceId, result.serviceConfig, result.apiCallSucceeded)
          _ -> pure (externalReferenceId, sosServiceConfig, externalApiStatus)
      pure
        ( finalRef,
          kaptureTicketId,
          dbSosId,
          finalSvc <|> sosServiceConfig,
          finalStatus
        )
    API.KAPTURE -> do
      (finalSosId, finalTicketId) <- case mbRideCtx of
        Just rideCtx -> do
          (sId, tId) <- createTicketForNewSosShared h personData rideCtx mbExistingSos externalReferenceId req
          pure (Just sId, tId)
        Nothing -> pure (dbSosId, kaptureTicketId)
      pure
        ( externalReferenceId,
          finalTicketId <|> kaptureTicketId,
          finalSosId <|> dbSosId,
          sosServiceConfig,
          externalApiStatus
        )

-- | Helper: create a Kapture ticket + SOS row for a ride-based SOS, or
-- reactivate an existing SOS (and update its ticket) when one is found for
-- this ride. Mirrors rider's `createTicketForNewSos`.
createTicketForNewSosShared ::
  ( BeamFlow m r,
    IssueBeamFlow.BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasSosHandle r m
  ) =>
  SosServiceHandle m ->
  SosPersonData ->
  SosRideCtx ->
  Maybe DSos.Sos ->
  Maybe Text ->
  API.SosReq ->
  m (Id DSos.Sos, Maybe Text)
createTicketForNewSosShared h personData rideCtx mbExistingSos mbExternalRef req = do
  case mbExistingSos of
    Just existing -> do
      -- Reactivation path: flip status back to Pending + refresh updatedAt
      result <-
        createRideBasedSos
          personData.personId
          rideCtx.rideId
          personData.merchantOperatingCityId
          personData.merchantId
          req.flow
          (Just existing)
          existing.ticketId
          mbExternalRef
      whenJust existing.ticketId $ \ticketId ->
        h.callKaptureUpdateTicket
          personData.merchantId
          personData.merchantOperatingCityId
          Ticket.UpdateTicketReq
            { comment = "SOS Re-Activated",
              ticketId = ticketId,
              subStatus = Ticket.IN,
              rideDescription = Nothing,
              issueDetails = Nothing
            }
      pure (result.sosId, existing.ticketId)
    Nothing -> do
      -- Fresh ticket (gated by enableSupportForSafety) + fresh SOS row
      mbTicketId <-
        if personData.enableSupportForSafety
          then do
            let dashboardUrls =
                  buildDashboardMediaUrls
                    personData.dashboardMediaUrlPattern
                    (Just rideCtx.rideId.getId)
                    Nothing
                    personData.merchantShortId
                    rideCtx.rideCityCode
            h.callKaptureCreateTicket
              personData.merchantId
              personData.merchantOperatingCityId
              Ticket.CreateTicketReq
                { category = "Code Red",
                  subCategory = Just "SOS Alert (follow-back)",
                  issueId = Nothing,
                  issueDescription = buildSosIssueDescription personData.ticketClassification req.flow,
                  mediaFiles = Just dashboardUrls,
                  name = Just personData.personDisplayName,
                  phoneNo = personData.personMobile,
                  personId = personData.personId.getId,
                  classification = personData.ticketClassification,
                  rideDescription = rideCtx.rideInfo,
                  disposition = personData.kaptureDisposition,
                  queue = personData.kaptureQueue,
                  becknIssueId = Nothing
                }
          else pure Nothing
      result <-
        createRideBasedSos
          personData.personId
          rideCtx.rideId
          personData.merchantOperatingCityId
          personData.merchantId
          req.flow
          Nothing
          mbTicketId
          mbExternalRef
      pure (result.sosId, mbTicketId)

-- | Non-ride Kapture ticket: used when sosCreate is called without a rideId
-- and KAPTURE trigger fires. Uses `personCityCode` rather than a ride-scoped
-- city code because there is no ride.
createNonRideKaptureTicketShared ::
  ( HasSosHandle r m,
    MonadFlow m
  ) =>
  SosServiceHandle m ->
  SosPersonData ->
  Id Common.Person ->
  Id DSos.Sos ->
  DSos.SosType ->
  m (Maybe Text)
createNonRideKaptureTicketShared h personData personId sosId flow = do
  let dashboardUrls =
        buildDashboardMediaUrls
          personData.dashboardMediaUrlPattern
          Nothing
          (Just sosId.getId)
          personData.merchantShortId
          personData.personCityCode
  h.callKaptureCreateTicket
    personData.merchantId
    personData.merchantOperatingCityId
    Ticket.CreateTicketReq
      { category = "Code Red",
        subCategory = Just "SOS Alert (follow-back)",
        issueId = Nothing,
        issueDescription = buildSosIssueDescription personData.ticketClassification flow,
        mediaFiles = Just dashboardUrls,
        name = Just personData.personDisplayName,
        phoneNo = personData.personMobile,
        personId = personId.getId,
        classification = personData.ticketClassification,
        rideDescription = Nothing,
        disposition = personData.kaptureDisposition,
        queue = personData.kaptureQueue,
        becknIssueId = Nothing
      }

-- | Mirror rider's check: when the request omits triggerApiList or provides an
-- empty list, the default behavior is KAPTURE. Otherwise, KAPTURE must be
-- present in the requested list.
triggerListIncludesKapture :: Maybe [API.TriggerApi] -> Bool
triggerListIncludesKapture = \case
  Nothing -> True
  Just [] -> True
  Just xs -> API.KAPTURE `elem` xs

-- | Update an SOS row's status (valid transitions are enforced by the shared
-- `updateSosStatusWithCache` helper). If the SOS has a Kapture ticket, fire
-- the ticket update callback so the external ticket reflects the new state.
sosStatus ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  Id DSos.Sos ->
  API.SosUpdateReq ->
  m APISuccess
sosStatus authToken sosId req = do
  personId <- extractSosPersonId authToken
  updatedSos <- updateSosStatusWithCache sosId req.status personId
  h <- getSosHandle
  personData <- h.getPersonData personId
  whenJust updatedSos.ticketId $ \ticketId ->
    h.callKaptureUpdateTicket
      personData.merchantId
      personData.merchantOperatingCityId
      Ticket.UpdateTicketReq
        { comment = fromMaybe "" req.comment,
          ticketId = ticketId,
          subStatus = Ticket.IN,
          rideDescription = Nothing,
          issueDetails = Nothing
        }
  pure Success

-- | Resolve an SOS. Two paths:
--   * isMock == Just True → mark the mock-drill Redis entry as resolved and
--     fire a single "resolved safe" notification. enableMockDrill gate falls
--     through on driver (no mock key exists there).
--   * otherwise → shared 'markSosAsSafe' helper handles DB + cache, then:
--     - Kapture ticket update if ticket present
--     - Clear live-tracking Redis location if tracking was stopped
--     - Notify emergency contacts if the shared helper says so
sosMarkRideAsSafe ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  Id DSos.Sos ->
  API.MarkAsSafeReq ->
  m APISuccess
sosMarkRideAsSafe authToken sosId API.MarkAsSafeReq {..} = do
  personId <- extractSosPersonId authToken
  h <- getSosHandle
  personData <- h.getPersonData personId
  emergencyContacts <- getSosEmergencyContactsShared personId
  let contactsToNotify =
        case contacts of
          Nothing -> emergencyContacts
          Just contactsList ->
            if L.null contactsList
              then []
              else L.filter (\ec -> L.elem ec.mobileNumber contactsList) emergencyContacts
  case isMock of
    Just True -> do
      when h.getPlatformConfig.enableMockDrill $ do
        (mbMock :: Maybe DSos.SosMockDrill) <- Redis.safeGet (CQSos.mockSosKey personId)
        whenJust mbMock $ \_ ->
          CQSos.setMockDrill
            personId
            DSos.SosMockDrill {personId = personId, status = DSos.MockResolved}
      h.sendSosNotification
        personData
        (Left "SOS_RESOLVED_SAFE")
        Notification.SOS_RESOLVED
        [("userName", personData.personDisplayName)]
        Nothing
        False
        emergencyContacts
        Nothing
      pure Success
    _ -> do
      result <- markSosAsSafe sosId personId isEndLiveTracking isRideEnded
      -- Kapture ticket update (unconditional when ticketId present)
      whenJust result.updatedSos.ticketId $ \ticketId ->
        h.callKaptureUpdateTicket
          personData.merchantId
          personData.merchantOperatingCityId
          Ticket.UpdateTicketReq
            { comment = "Mark Ride as Safe",
              ticketId = ticketId,
              subStatus = Ticket.IN,
              rideDescription = Nothing,
              issueDetails = Nothing
            }
      -- Clear live-tracking location if tracking is stopping
      when result.shouldStopTracking $
        CQSosLocation.clearSosLocation sosId
      -- Notify contacts if the shared helper decided so
      when result.shouldNotifyContacts $ do
        let mbSosIdForNotification = if result.isRideBased then Nothing else Just sosId
        h.sendSosNotification
          personData
          (Left result.notificationKey)
          Notification.SOS_RESOLVED
          [("userName", personData.personDisplayName)]
          Nothing
          False
          contactsToNotify
          mbSosIdForNotification
      pure Success

-- | Mock safety drill. Two sub-flows depending on the `startDrill` field on
-- the request:
--   * Just True  → "about to start a drill" — notify contacts with a warm-up
--     body/title (no mock Redis entry created yet; optionally flip share-ride
--     + enable follow-ride if the request is `onRide`).
--   * otherwise  → drill actually running — mark the person as having
--     completed a drill (first time only), stash a MockPending Redis entry if
--     `onRide`, and notify contacts with the "drill in progress" body/title.
-- Body/title strings live here (not rider's local `where` clause — see Step 1
-- pure-helpers move).
sosCreateMockSos ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  API.MockSosReq ->
  m APISuccess
sosCreateMockSos authToken API.MockSosReq {..} = do
  h <- getSosHandle
  unless h.getPlatformConfig.enableMockDrill $
    throwError $ InvalidRequest "Mock drill is not supported on this platform"
  personId <- extractSosPersonId authToken
  personData <- h.getPersonData personId
  emergencyContacts <- getSosEmergencyContactsShared personId
  safetySettings <-
    QSafetyExtra.findSafetySettingsWithFallback
      personId
      (QSafetyExtra.getDefaultSafetySettings personId personData.safetySettingsDefaults)
  case startDrill of
    Just True -> do
      h.sendSosNotification
        personData
        (Right (mockDrillBody personData.personDisplayName True, mockDrillTitle))
        Notification.SOS_MOCK_DRILL_NOTIFY
        []
        Nothing
        False
        emergencyContacts
        Nothing
      when (fromMaybe False onRide) $ do
        when h.getPlatformConfig.enableShareRide $
          SafetyQPDEN.updateShareRideForAll personId True
        when h.getPlatformConfig.enableFollowRide $
          h.enableFollowRideForContacts personId emergencyContacts
    _ -> do
      unless (fromMaybe False safetySettings.hasCompletedMockSafetyDrill) $
        updateMockSafetyDrillStatus (Just True) personId
      when (fromMaybe False onRide) $
        CQSos.setMockDrill
          personId
          DSos.SosMockDrill {personId = personId, status = DSos.MockPending}
      h.sendSosNotification
        personData
        (Right (mockDrillBody personData.personDisplayName False, mockDrillTitle))
        Notification.SOS_MOCK_DRILL
        []
        Nothing
        False
        emergencyContacts
        Nothing
  pure Success
  where
    mockDrillTitle :: Text
    mockDrillTitle = "Test Safety Drill Alert"

    mockDrillBody :: Text -> Bool -> Text
    mockDrillBody displayName isStartDrill =
      displayName
        <> if isStartDrill
          then " is going to start a test safety drill with you. Tap to follow the test ride. This is a practice exercise, and not a real ride."
          else " has initiated a test safety drill with you. This is a practice exercise, not a real emergency situation..."

-- | Trigger a police call (rider-only; gated by PlatformConfig.enablePoliceCall).
-- Delegates the incident-report flow to the app callback so we don't need to
-- pull the rider-specific IncidentReport / scheduler machinery into shared-services.
sosCallPolice ::
  ( MonadFlow m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  API.CallPoliceAPI ->
  m APISuccess
sosCallPolice authToken req = do
  personId <- extractSosPersonId authToken
  h <- getSosHandle
  unless h.getPlatformConfig.enablePoliceCall $
    throwError $ InvalidRequest "Police call is not supported on this platform"
  personData <- h.getPersonData personId
  h.triggerPoliceCall personData req.rideId

-- | Stream a rider's current location for an active tracking SOS. Validates
-- personId match and that the SOS is still Pending. Forks the external-SOS
-- trace (rider-only — driver's callback is a no-op).
sosUpdateLocation ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  Id DSos.Sos ->
  API.SosLocationUpdateReq ->
  m APISuccess
sosUpdateLocation authToken sosId req = do
  h <- getSosHandle
  unless h.getPlatformConfig.enableShareRide $
    throwError $ InvalidRequest "Feature not supported on this platform"
  personId <- extractSosPersonId authToken
  sosDetails <-
    B.runInReplica $
      QSos.findById sosId >>= fromMaybeM (SosNotFound sosId.getId)
  unless (personId == sosDetails.personId) $
    throwError $ InvalidRequest "PersonId not same"
  unless (sosDetails.status == DSos.Pending) $
    throwError $ InvalidRequest "Can only update location for pending SOS"
  CQSosLocation.updateSosLocation
    sosId
    LatLong {lat = req.lat, lon = req.lon}
    req.accuracy
    sosDetails.trackingExpiresAt
  fork "sendExternalSOSTrace" $ h.sendExternalSosTrace sosDetails req
  pure Success

-- | No-auth read of the live tracking state for a non-ride SafetyFlow SOS.
-- Gated by PlatformConfig.enableShareRide (driver: False → 4xx).
sosTracking ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasSosHandle r m
  ) =>
  Id DSos.Sos ->
  m API.SosTrackingRes
sosTracking sosId = do
  h <- getSosHandle
  unless h.getPlatformConfig.enableShareRide $
    throwError $ InvalidRequest "Feature not supported on this platform"
  checkSlidingWindowLimitWithOptions
    (CQSos.sosTrackingHitsCountKey sosId)
    h.getPlatformConfig.sosTrackingRateLimitOptions
  sosDetails <-
    B.runInReplica $
      QSos.findById sosId >>= fromMaybeM (SosNotFound sosId.getId)
  unless (sosDetails.flow == DSos.SafetyFlow && sosDetails.entityType == Just DSos.NonRide) $
    throwError $ InvalidRequest "Invalid SOS for tracking"
  now <- getCurrentTime
  let isTrackingActive =
        sosDetails.status == DSos.Pending && case sosDetails.trackingExpiresAt of
          Just expiry -> expiry > now
          Nothing -> False
  mbLocation <-
    if isTrackingActive
      then CQSosLocation.getSosLocation sosId
      else pure Nothing
  pure
    API.SosTrackingRes
      { currentLocation = toApiLocation <$> mbLocation,
        sosState = sosDetails.sosState,
        status = sosDetails.status
      }
  where
    toApiLocation :: CQSosLocation.SosLocationData -> API.SosLocationRes
    toApiLocation loc =
      API.SosLocationRes
        { lat = loc.lat,
          lon = loc.lon,
          accuracy = loc.accuracy
        }

-- | Start a non-ride SOS live-tracking session. Creates (or updates the
-- expiry on) a Pending non-ride SOS, seeds its live location if provided,
-- and forks a "LIVE_TRACKING_STARTED" push to the caller-specified contacts.
sosStartTracking ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  API.StartTrackingReq ->
  m API.StartTrackingRes
sosStartTracking authToken API.StartTrackingReq {..} = do
  h <- getSosHandle
  unless h.getPlatformConfig.enableShareRide $
    throwError $ InvalidRequest "Feature not supported on this platform"
  personId <- extractSosPersonId authToken
  when (durationInMinutes < 1 || durationInMinutes > 1440) $
    throwError $ InvalidRequest "Duration must be between 1 and 1440 minutes (24 hours)"
  personData <- h.getPersonData personId
  now <- getCurrentTime
  let expiryTimeStamp = addUTCTime (fromIntegral (durationInMinutes * 60)) now
  finalSosId <-
    startSosTracking
      sosId
      personId
      (Just personData.merchantId)
      (Just personData.merchantOperatingCityId)
      expiryTimeStamp
      externalReferenceId
      DSos.SafetyFlow
  whenJust customerLocation $ \location ->
    CQSosLocation.updateSosLocation finalSosId location Nothing (Just expiryTimeStamp)
  let trackLink = h.buildSosTrackingUrl personData Nothing finalSosId
  allEmergencyContacts <- getSosEmergencyContactsShared personId
  let contactsToNotify =
        if L.null contacts
          then []
          else L.filter (\ec -> L.elem ec.mobileNumber contacts) allEmergencyContacts
  when (not (L.null contacts) && L.null contactsToNotify) $
    throwError $ InvalidRequest "No valid emergency contacts found"
  when (not (L.null contactsToNotify)) $
    fork "Notifying emergency contacts about live tracking start" $ do
      let formattedExpiryTime = T.pack $ formatTime defaultTimeLocale "%I:%M %p" expiryTimeStamp
      h.sendSosNotification
        personData
        (Left "LIVE_TRACKING_STARTED")
        Notification.SHARE_RIDE
        [("userName", personData.personDisplayName), ("expiryTime", formattedExpiryTime)]
        Nothing
        False
        contactsToNotify
        (Just finalSosId)
  pure API.StartTrackingRes {sosId = finalSosId, trackingUrl = trackLink}

-- | Transition an SOS between states. Two notable transitions:
--   * LiveTracking → SosActive: opens a Kapture ticket if none exists, extends
--     tracking expiry by 8h (inside updateSosStateWithAutoExpiry), refreshes the
--     live location with the new expiry, and notifies emergency contacts.
--   * SosActive → LiveTracking: notifies contacts with a "resolved" key.
-- No-op (HTTP 200) if the SOS is already in the requested state.
sosUpdateState ::
  ( BeamFlow m r,
    IssueBeamFlow.BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  Id DSos.Sos ->
  API.UpdateStateReq ->
  m APISuccess
sosUpdateState authToken sosId API.UpdateStateReq {..} = do
  h <- getSosHandle
  unless h.getPlatformConfig.enableShareRide $
    throwError $ InvalidRequest "Feature not supported on this platform"
  personId <- extractSosPersonId authToken
  sosDetails <-
    B.runInReplica $
      QSos.findById sosId >>= fromMaybeM (SosNotFound sosId.getId)
  if sosDetails.sosState == Just sosState
    then pure Success
    else do
      personData <- h.getPersonData personId
      emergencyContacts <- getSosEmergencyContactsShared personId
      mbRideCtxForUrl <- traverse h.getRideCtx sosDetails.rideId
      let trackLink = h.buildSosTrackingUrl personData mbRideCtxForUrl sosId
      _updatedSos <- case (sosDetails.sosState, sosState) of
        (Just DSos.LiveTracking, DSos.SosActive) -> do
          -- Open a Kapture ticket if the SOS doesn't already have one
          mbNewTicketId <-
            if personData.enableSupportForSafety && isNothing sosDetails.ticketId
              then createNonRideKaptureTicketShared h personData personId sosId sosDetails.flow
              else pure Nothing
          result <- updateSosStateWithAutoExpiry personId sosDetails sosState mbNewTicketId
          -- Refresh live-tracking expiry using the latest known location
          let newExpiryTime = result.trackingExpiresAt
          mbCurrentLocation <- CQSosLocation.getSosLocation sosId
          whenJust mbCurrentLocation $ \locationData ->
            whenJust newExpiryTime $ \expiry ->
              CQSosLocation.updateSosLocation
                sosId
                LatLong {lat = locationData.lat, lon = locationData.lon}
                locationData.accuracy
                (Just expiry)
          -- Notify contacts about the escalation
          fork "postSosUpdateState:notifyEmergencyContacts" $ do
            let alertParams =
                  SosAlertParams
                    { userName = personData.personDisplayName,
                      rideLink = trackLink,
                      rideEndTime = Nothing,
                      isRideEnded = False
                    }
            h.sendSosNotification
              personData
              (Left "SOS_ALERT")
              Notification.SOS_TRIGGERED
              [("userName", personData.personDisplayName)]
              (Just alertParams)
              True
              emergencyContacts
              (Just sosId)
          pure result
        (Just DSos.SosActive, DSos.LiveTracking) -> do
          result <- updateSosStateWithAutoExpiry personId sosDetails sosState Nothing
          h.sendSosNotification
            personData
            (Left "SOS_RESOLVED")
            Notification.SOS_RESOLVED
            [("userName", personData.personDisplayName)]
            Nothing
            False
            emergencyContacts
            (Just sosId)
          pure result
        _ ->
          updateSosStateWithAutoExpiry personId sosDetails sosState Nothing
      -- Update Kapture ticket only if it existed BEFORE this request. If the
      -- ticket was just created in the LiveTracking → SosActive branch, skip
      -- the immediate update to avoid a redundant comment on a brand-new ticket.
      whenJust sosDetails.ticketId $ \ticketId ->
        h.callKaptureUpdateTicket
          personData.merchantId
          personData.merchantOperatingCityId
          Ticket.UpdateTicketReq
            { comment = "SOS State Updated",
              ticketId = ticketId,
              subStatus = Ticket.IN,
              rideDescription = Nothing,
              issueDetails = Nothing
            }
      pure Success

-- | Return the SOS creator's display name + phone number. Access allowed only
-- to the creator or one of their emergency contacts (per rider's existing
-- access check). Phone comes from SosPersonData.personMobile (already
-- decrypted), so no encryption plumbing needed here.
sosTrackingDetails ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  Id DSos.Sos ->
  m API.SosTrackingDetailsRes
sosTrackingDetails authToken sosId = do
  h <- getSosHandle
  unless h.getPlatformConfig.enableShareRide $
    throwError $ InvalidRequest "Feature not supported on this platform"
  callerPersonId <- extractSosPersonId authToken
  sosDetails <-
    B.runInReplica $
      QSos.findById sosId >>= fromMaybeM (SosNotFound sosId.getId)
  unless (sosDetails.flow == DSos.SafetyFlow && sosDetails.entityType == Just DSos.NonRide) $
    throwError $ InvalidRequest "Invalid SOS for tracking"
  unless (sosDetails.status == DSos.Pending) $
    throwError $ InvalidRequest "Location sharing has expired"
  -- Access check: caller is either the SOS creator or one of their emergency contacts
  let isSosCreator = callerPersonId == sosDetails.personId
  isEmergencyContact <-
    if isSosCreator
      then pure True
      else do
        contacts <- B.runInReplica $ SafetyQPDEN.findAllByPersonId sosDetails.personId
        pure $ Foldable.any (\ec -> ec.contactPersonId == Just (cast callerPersonId)) contacts
  unless isEmergencyContact $
    throwError $ InvalidRequest "Access denied"
  creatorData <- h.getPersonData sosDetails.personId
  phoneNumber <-
    creatorData.personMobile & fromMaybeM (InvalidRequest "Person mobile not present")
  pure
    API.SosTrackingDetailsRes
      { personName = creatorData.personName,
        mobileNumber = phoneNumber
      }

-- | Convert a non-ride SOS to ride-scoped by associating it with an existing
-- ride. Validates ownership on both sides (SOS creator === caller, and ride
-- owner === caller via SosRideCtx.ownerPersonId). If a Kapture ticket exists,
-- update it; otherwise create a fresh one and kick off contact notifications.
sosUpdateToRide ::
  ( BeamFlow m r,
    IssueBeamFlow.BeamFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  Id DSos.Sos ->
  API.UpdateToRideReq ->
  m APISuccess
sosUpdateToRide authToken sosId API.UpdateToRideReq {..} = do
  h <- getSosHandle
  unless h.getPlatformConfig.enableShareRide $
    throwError $ InvalidRequest "Feature not supported on this platform"
  personId <- extractSosPersonId authToken
  sosDetails <-
    B.runInReplica $
      QSos.findById sosId >>= fromMaybeM (SosNotFound sosId.getId)
  unless (personId == sosDetails.personId) $
    throwError $ InvalidRequest "PersonId not same"
  unless (sosDetails.entityType == Just DSos.NonRide) $
    throwError $ InvalidRequest "Can only update non-ride SOS to ride"
  rideCtx <- h.getRideCtx rideId
  unless (personId == rideCtx.ownerPersonId) $
    throwError $ InvalidRequest "Ride does not belong to this person"
  void $ updateSosFromNonRideToRide sosDetails rideId
  personData <- h.getPersonData personId
  let trackLink = h.buildSosTrackingUrl personData (Just rideCtx) sosId
  when personData.enableSupportForSafety $ do
    let dashboardUrls =
          buildDashboardMediaUrls
            personData.dashboardMediaUrlPattern
            (Just rideCtx.rideId.getId)
            (Just sosId.getId)
            personData.merchantShortId
            rideCtx.rideCityCode
        mediaLinks = [trackLink] <> dashboardUrls
    case sosDetails.ticketId of
      Just existingTicketId ->
        -- Update the pre-existing non-ride Kapture ticket with ride context
        h.callKaptureUpdateTicket
          personData.merchantId
          personData.merchantOperatingCityId
          Ticket.UpdateTicketReq
            { comment = "SOS converted from non-ride to ride",
              ticketId = existingTicketId,
              subStatus = Ticket.IN,
              rideDescription = rideCtx.rideInfo,
              issueDetails = Nothing
            }
      Nothing -> do
        -- No existing ticket: create a fresh ride-scoped ticket and notify contacts
        mbNewTicketId <-
          h.callKaptureCreateTicket
            personData.merchantId
            personData.merchantOperatingCityId
            Ticket.CreateTicketReq
              { category = "Code Red",
                subCategory = Just "SOS Alert (follow-back)",
                issueId = Nothing,
                issueDescription = buildSosIssueDescription personData.ticketClassification DSos.SafetyFlow,
                mediaFiles = Just mediaLinks,
                name = Just personData.personDisplayName,
                phoneNo = personData.personMobile,
                personId = personId.getId,
                classification = personData.ticketClassification,
                rideDescription = rideCtx.rideInfo,
                disposition = personData.kaptureDisposition,
                queue = personData.kaptureQueue,
                becknIssueId = Nothing
              }
        whenJust mbNewTicketId $ \newTicketId ->
          void $ updateSosTicketId sosDetails (Just newTicketId)
        safetySettings <-
          QSafetyExtra.findSafetySettingsWithFallback
            personId
            (QSafetyExtra.getDefaultSafetySettings personId personData.safetySettingsDefaults)
        when safetySettings.notifySosWithEmergencyContacts $
          fork "postSosUpdateToRide:notifyEmergencyContacts" $ do
            let alertParams =
                  SosAlertParams
                    { userName = personData.personDisplayName,
                      rideLink = trackLink,
                      rideEndTime = T.pack . formatTime defaultTimeLocale "%e-%-m-%Y %-I:%M%P" <$> rideCtx.rideEndTime,
                      isRideEnded = False
                    }
            emergencyContacts <- getSosEmergencyContactsShared personId
            when h.getPlatformConfig.enableShareRide $
              SafetyQPDEN.updateShareRideForAll personId True
            when h.getPlatformConfig.enableFollowRide $
              h.enableFollowRideForContacts personId emergencyContacts
            h.sendSosNotification
              personData
              (Left "SOS_ALERT")
              Notification.SOS_TRIGGERED
              [("userName", personData.personDisplayName)]
              (Just alertParams)
              True
              emergencyContacts
              Nothing
  pure Success

-- | Look up the caller's SOS by status. externalSOSConfig is always Nothing
-- in this response (matches rider's pre-unification behaviour).
sosGetDetailsByPerson ::
  ( BeamFlow m r,
    CoreMetrics m,
    BuildSosCtx authToken m
  ) =>
  authToken ->
  DSos.SosStatus ->
  m API.SosDetailsRes
sosGetDetailsByPerson authToken status = do
  personId <- extractSosPersonId authToken
  mbSos <- QSos.findByPersonIdAndStatus personId status
  pure API.SosDetailsRes {sos = mbSos, externalSOSConfig = Nothing}

-- | Webhook called by the police/ERSS system to relay status updates for a
-- previously-triggered external SOS. No-auth (the ERSS vendor doesn't have our
-- tokens); rate-limited globally via CQSos.erssStatusUpdateHitsCountKey.
-- History is appended as a JSON array of 'ExternalStatusEntry'.
sosErssStatusUpdate ::
  ( BeamFlow m r,
    CoreMetrics m,
    HasSosHandle r m
  ) =>
  API.ErssStatusUpdateReq ->
  m API.ErssStatusUpdateRes
sosErssStatusUpdate req = do
  h <- getSosHandle
  checkSlidingWindowLimitWithOptions
    CQSos.erssStatusUpdateHitsCountKey
    h.getPlatformConfig.erssStatusUpdateRateLimitOptions
  mbSosDetails <- QSos.findByExternalReferenceId (Just req.idErss)
  case mbSosDetails of
    Nothing ->
      pure
        API.ErssStatusUpdateRes
          { resultCode = "OPERATION_FAILURE",
            resultString = Nothing,
            errorMsg = Just $ "No SOS found for tracking ID: " <> req.idErss,
            message = Nothing,
            payLoad = Nothing
          }
    Just sosDetails -> do
      let previousStatus = sosDetails.externalReferenceStatus
          newStatus = req.currentStatus
          newEntry =
            ExternalStatusEntry
              { status = newStatus,
                idErss = req.idErss,
                lastUpdatedTime = req.lastUpdatedTime
              }
          existingHistory :: [ExternalStatusEntry]
          existingHistory =
            maybe
              []
              (\raw -> fromMaybe [] (A.decode (LBS.fromStrict $ TE.encodeUtf8 raw)))
              sosDetails.externalStatusHistory
          updatedHistory = existingHistory <> [newEntry]
          updatedHistoryJson =
            Just $ TE.decodeUtf8 $ LBS.toStrict $ A.encode updatedHistory
      QSos.updateExternalReferenceStatus
        (Just newStatus)
        updatedHistoryJson
        sosDetails.id
      logInfo $
        "ERSS status update received for SOS " <> sosDetails.id.getId
          <> ": "
          <> fromMaybe "none" previousStatus
          <> " -> "
          <> newStatus
      pure
        API.ErssStatusUpdateRes
          { resultCode = "OPERATION_SUCCESS",
            resultString = Just "Status update received successfully",
            errorMsg = Nothing,
            message = Nothing,
            payLoad = Nothing
          }

-- | Public (no-auth) ride details used by the driver UI. Rider's callback
-- impl is a stub that throws (see §4 of the plan doc); driver returns real data.
sosRideDetails ::
  ( MonadFlow m,
    HasSosHandle r m
  ) =>
  ShortId Common.Ride ->
  m API.RideDetailsForDriverRes
sosRideDetails rideShortId = do
  h <- getSosHandle
  h.getSosRideDetails rideShortId

-- | Upload a media file (audio/video) against an active SOS.
-- Pipeline: validate file size → upload to S3 → store a MediaFile row →
-- attach it to the SOS → (if enabled) attach media link to the Kapture ticket →
-- (if enabled) upload to the external SOS service for police/ERSS.
sosUploadMedia ::
  ( BeamFlow m r,
    IssueBeamFlow.BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    MonadReader r m,
    HasField "s3Env" r (S3.S3Env m),
    BuildSosCtx authToken m,
    HasSosHandle r m
  ) =>
  authToken ->
  Id DSos.Sos ->
  SOSVideoUploadReq ->
  m AddSosVideoRes
sosUploadMedia authToken sosId req = do
  personId <- extractSosPersonId authToken
  h <- getSosHandle
  personData <- h.getPersonData personId
  sosDetails <-
    B.runInReplica $
      QSos.findById sosId >>= fromMaybeM (SosNotFound sosId.getId)
  -- 1. Validate file size
  fileSize <- EL.runIO $ withFile req.payload ReadMode hFileSize
  when (fileSize > personData.mediaFileSizeLimit) $
    throwError $ FileSizeExceeded (show fileSize)
  -- 2. S3 upload
  mediaFile <- EL.runIO $ base64Encode <$> BS.readFile req.payload
  filePath <- S3.createFilePath "/sos/" ("sos-" <> sosId.getId) req.fileType req.fileExtension
  mediaFileId <- generateGUID
  now <- getCurrentTime
  let fileUrl =
        personData.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "sos"
          & T.replace "<FILE_PATH>" filePath
      fileEntity =
        DMF.MediaFile
          { id = mediaFileId,
            _type = req.fileType,
            url = fileUrl,
            s3FilePath = Just filePath,
            createdAt = now
          }
  s3Res <- withTryCatch "S3:put:uploadSosMedia" $ S3.put (T.unpack filePath) mediaFile
  case s3Res of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      MFQuery.create fileEntity
      let updatedMediaFiles = sosDetails.mediaFiles <> [mediaFileId]
      void $ updateSosMediaFiles updatedMediaFiles sosId
      -- 3. Kapture ticket update (ride-based + support flag on)
      when (personData.enableSupportForSafety && isRideBasedSos sosDetails.entityType) $ do
        whenJust sosDetails.rideId $ \rideIdInner -> do
          rideCtx <- h.getRideCtx rideIdInner
          let dashboardUrls =
                buildDashboardMediaUrls
                  personData.dashboardMediaUrlPattern
                  (Just rideIdInner.getId)
                  (Just sosId.getId)
                  personData.merchantShortId
                  rideCtx.rideCityCode
          case sosDetails.ticketId of
            Just ticketId ->
              h.callKaptureUpdateTicket
                personData.merchantId
                personData.merchantOperatingCityId
                Ticket.UpdateTicketReq
                  { comment = "Audio recording/shared media uploaded.",
                    ticketId = ticketId,
                    subStatus = Ticket.IN,
                    rideDescription = Nothing,
                    issueDetails =
                      Just
                        Ticket.UpdateIssueDetails
                          { mediaFiles = Just dashboardUrls,
                            issueDescription = Nothing,
                            issueId = Nothing,
                            subCategory = Nothing,
                            vehicleCategory = Nothing,
                            category = Nothing
                          }
                  }
            Nothing -> do
              -- Fallback: SOS has no Kapture ticket (initial create failed
              -- during sosCreate). Create a fresh ticket now with the media
              -- link attached, and persist the new ticketId on the SOS row.
              mbNewTicketId <-
                h.callKaptureCreateTicket
                  personData.merchantId
                  personData.merchantOperatingCityId
                  Ticket.CreateTicketReq
                    { category = "Code Red",
                      subCategory = Just "SOS Alert (follow-back)",
                      issueId = Nothing,
                      issueDescription = "Audio recording shared.",
                      mediaFiles = Just dashboardUrls,
                      name = Just personData.personDisplayName,
                      phoneNo = personData.personMobile,
                      personId = personId.getId,
                      classification = personData.ticketClassification,
                      rideDescription = rideCtx.rideInfo,
                      disposition = personData.kaptureDisposition,
                      queue = personData.kaptureQueue,
                      becknIssueId = Nothing
                    }
              whenJust mbNewTicketId $ \newTicketId ->
                void $ updateSosTicketId sosDetails (Just newTicketId)
      -- 4. External SOS media upload (rider-only in practice; gated)
      whenJust personData.externalSOSConfig $ \sosConfig ->
        when (sosConfig.mediaRequired && h.getPlatformConfig.enableExternalSos) $
          whenJust sosDetails.externalReferenceId $ \_ ->
            whenJust personData.personMobile $ \phoneNo -> do
              let fileName = "sos-" <> sosId.getId <> "." <> req.fileExtension
              h.uploadExternalSosMedia personData sosId phoneNo req.payload fileName
      pure $ AddSosVideoRes {fileUrl = fileUrl}
