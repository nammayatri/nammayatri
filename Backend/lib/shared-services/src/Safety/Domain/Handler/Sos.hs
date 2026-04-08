{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Safety.Domain.Handler.Sos where

import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Safety.Common.Builder
import Safety.Common.Handle
import Safety.Common.Types (PersonE (..))
import qualified Safety.Domain.Action.UI.Sos as Action
import qualified Safety.Domain.Types.Common as Common
import qualified Safety.Domain.Types.Sos as DSos
import Safety.Storage.BeamFlow

-- | Convenience: withSafety specialized with BuildSafetyCtx
runSafety ::
  forall r authToken m a.
  (Monad m, HasSafetyHandle r m, BuildSafetyCtx authToken m) =>
  authToken ->
  (SafetyMonad m -> m a) ->
  m a
runSafety = withSafety @r buildSafetyCtx

-- ---------------------------------------------------------------------------
-- Shared handler functions
-- ---------------------------------------------------------------------------

-- | GET /sos/getDetails/{rideId}
getSosGetDetails ::
  forall r m authToken.
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  Id Common.Ride ->
  m (Maybe DSos.Sos)
getSosGetDetails auth rideId = runSafety @r auth $ \_env ->
  Action.findSosByRideId rideId

-- | POST /sos/create
postSosCreate ::
  forall r m authToken.
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  DSos.SosType ->
  Maybe (Id Common.Ride) ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  m Action.CreateSosResult
postSosCreate auth flow mbRideId _mbIsRideEnded _mbSendPN _mbNotifyAll =
  runSafety @r auth $ \env -> do
    case mbRideId of
      Just rideId -> do
        merchantOpCityId <- env.ctx.merchantOpCityId & fromMaybeM (InvalidRequest "merchantOpCityId required for ride-based SOS")
        Action.createRideBasedSos
          env.ctx.personId
          rideId
          merchantOpCityId
          env.ctx.merchantId
          flow
          Nothing
          Nothing
          Nothing
      Nothing -> do
        now <- getCurrentTime
        let eightHoursInSeconds :: Int = 8 * 60 * 60
        let trackingExpiresAt = addUTCTime (fromIntegral eightHoursInSeconds) now
        sos <-
          Action.createNonRideSos
            env.ctx.personId
            (Just env.ctx.merchantId)
            env.ctx.merchantOpCityId
            (Just trackingExpiresAt)
            Nothing
            flow
        pure
          Action.CreateSosResult
            { sosId = sos.id,
              wasReactivated = False,
              sosDetails = sos
            }

-- | POST /sos/{sosId}/status
postSosStatus ::
  forall r m authToken.
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  Id DSos.Sos ->
  DSos.SosStatus ->
  m DSos.Sos
postSosStatus auth sosId status = runSafety @r auth $ \env ->
  Action.updateSosStatusWithCache sosId status env.ctx.personId

-- | POST /sos/markRideAsSafe/{sosId}
postSosMarkRideAsSafe ::
  forall r m authToken.
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  Id DSos.Sos ->
  Maybe Bool ->
  Maybe Bool ->
  m Action.MarkSosAsSafeResult
postSosMarkRideAsSafe auth sosId mbIsEndLiveTracking mbIsRideEnded =
  runSafety @r auth $ \env -> do
    result <- Action.markSosAsSafe sosId env.ctx.personId mbIsEndLiveTracking mbIsRideEnded
    when result.shouldNotifyContacts $
      notifyEmergencyContacts env.handle env.ctx $
        NotifyContactsReq
          { sosId = sosId,
            notificationKey = result.notificationKey,
            contacts = Nothing
          }
    when result.shouldMarkAsResolved $
      onSosResolved env.handle env.ctx sosId
    pure result

-- | POST /sos/{sosId}/updateLocation
postSosUpdateLocation ::
  forall r m authToken.
  ( BeamFlow m r,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  Id DSos.Sos ->
  m ()
postSosUpdateLocation auth _sosId = runSafety @r auth $ \_env ->
  pure ()

-- | GET /sos/{sosId}/tracking — NoAuth
getSosTracking ::
  ( BeamFlow m r,
    CoreMetrics m
  ) =>
  Id DSos.Sos ->
  m (Maybe DSos.Sos)
getSosTracking = Action.findSosById

-- | POST /sos/startTracking
postSosStartTracking ::
  forall r m authToken.
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  Maybe (Id DSos.Sos) ->
  UTCTime ->
  Maybe Text ->
  DSos.SosType ->
  m (Id DSos.Sos)
postSosStartTracking auth mbSosId trackingExpiresAt mbExternalReferenceId flow =
  runSafety @r auth $ \env ->
    Action.startSosTracking
      mbSosId
      env.ctx.personId
      (Just env.ctx.merchantId)
      env.ctx.merchantOpCityId
      trackingExpiresAt
      mbExternalReferenceId
      flow

-- | POST /sos/updateState/{sosId}
postSosUpdateState ::
  forall r m authToken.
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    MonadTime m,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  Id DSos.Sos ->
  DSos.SosState ->
  m DSos.Sos
postSosUpdateState auth sosId newState = runSafety @r auth $ \env -> do
  sosData <- Action.findSosById sosId >>= fromMaybeM (InvalidRequest $ "SOS not found: " <> sosId.getId)
  Action.updateSosStateWithAutoExpiry env.ctx.personId sosData newState

-- | GET /sos/trackingDetails/{sosId}
getSosTrackingDetails ::
  forall r m authToken.
  ( BeamFlow m r,
    CoreMetrics m,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  Id DSos.Sos ->
  m (Maybe PersonE)
getSosTrackingDetails auth sosId = runSafety @r auth $ \env -> do
  sosData <- Action.findSosById sosId >>= fromMaybeM (InvalidRequest $ "SOS not found: " <> sosId.getId)
  findPersonById env.handle sosData.personId

-- ---------------------------------------------------------------------------
-- Rider-only handler functions
-- ---------------------------------------------------------------------------

-- | POST /sos/createMockSos — rider only
postSosCreateMockSos ::
  forall r m authToken.
  ( BeamFlow m r,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  m ()
postSosCreateMockSos auth = runSafety @r auth $ \_env ->
  pure ()

-- | POST /sos/{sosId}/updateToRide — rider only
postSosUpdateToRide ::
  forall r m authToken.
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasSafetyHandle r m,
    BuildSafetyCtx authToken m
  ) =>
  authToken ->
  Id DSos.Sos ->
  Id Common.Ride ->
  m ()
postSosUpdateToRide auth sosId rideId = runSafety @r auth $ \env -> do
  Action.updateSosFromNonRideToRide sosId rideId
  void $
    createSosTicket env.handle env.ctx $
      SosTicketReq
        { sosId = sosId,
          rideId = Just rideId,
          sosType = DSos.SafetyFlow,
          description = Just "Non-ride SOS upgraded to ride"
        }
