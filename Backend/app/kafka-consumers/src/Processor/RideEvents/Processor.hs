-- | Top-level fan-out processor for RideEndedEvent.
--
-- Each handler is wrapped in an idempotency guard so re-delivery (XCLAIM after idle,
-- consumer restart) is a no-op. Handler failures propagate up to the transport, which
-- decides whether to retry based on per-entry delivery count from XPENDING.
--
-- The real per-handler logic lives in driver-app's `SharedLogic.RideEvents.Handlers`
-- (imported via PackageImports). Each P1b-X PR fills in one of those handler bodies
-- while leaving this file unchanged.
module Processor.RideEvents.Processor
  ( processRideEnded,
  )
where

import "dynamic-offer-driver-app" Domain.Types.Event.RideEndedEvent (RideEndedEvent)
import "dynamic-offer-driver-app" Domain.Types.Extra.LeanFlow (LeanFlowFeature (..))
import Environment
import Kernel.Prelude
import Kernel.Utils.Common (withLogTag, withTryCatch)
import Kernel.Utils.DatastoreLatencyCalculator (withTimeGeneric)
import qualified Processor.RideEvents.Handlers as Handlers
import qualified Processor.RideEvents.Idempotency as Idem
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.SystemConfigs.LeanFlow as CQLF

processRideEnded :: RideEndedEvent -> Flow ()
processRideEnded event =
  withLogTag ("rideId-" <> event.rideId) $
    -- Event-level latency: total wall-clock to process one RideEndedEvent.
    -- producer_operation_duration{operation="rs-event:RideEndedEvent"} (seconds).
    void $
      withTimeGeneric "rs-event:RideEndedEvent" $ do
        runHandlerAlways "publishToAnalyticsKafka" event Handlers.handleAnalyticsKafka
        runHandlerAlways "publishRideInterpolation" event Handlers.handleRideInterpolation
        runHandler FLEET_OPERATOR_STATS "updateFleetAndOperatorStats" event Handlers.handleFleetOperatorStats
        runHandlerAlways "checkGpsTollBehavior" event Handlers.handleGpsTollBehavior
        runHandlerAlways "incrementRCStatsAndReminders" event Handlers.handleRCStatsReminders
        runHandlerAlways "notifyRideEnded" event Handlers.handleRideEndNotifications
        runHandler LEADERBOARD "updateLeaderboard" event Handlers.handleLeaderboard
        runHandler REFERRAL "sendReferralAndDriverToDriverReward" event Handlers.handleReferral
        runHandlerAlways "migrateDriverOperatingCity" event Handlers.handleDriverCityMigration

runHandler :: LeanFlowFeature -> Text -> RideEndedEvent -> (RideEndedEvent -> Flow ()) -> Flow ()
runHandler feature name event handler =
  withLogTag name $ do
    excluded <- CQLF.isFeatureExcluded feature
    unless excluded $ runHandlerBody name event handler

runHandlerAlways :: Text -> RideEndedEvent -> (RideEndedEvent -> Flow ()) -> Flow ()
runHandlerAlways name event handler = withLogTag name $ runHandlerBody name event handler

runHandlerBody :: Text -> RideEndedEvent -> (RideEndedEvent -> Flow ()) -> Flow ()
runHandlerBody name event handler =
  Idem.withIdempotency name event.rideId $ do
    (result, _ms) <- withTimeGeneric ("rs-handler:" <> name) (withTryCatch ("rs-handler:" <> name) (handler event))
    case result of
      Right () -> pure ()
      Left e -> throwM e
