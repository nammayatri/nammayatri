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
import Environment
import Kernel.Prelude
import Kernel.Utils.Common (withLogTag, withTryCatch)
import Kernel.Utils.DatastoreLatencyCalculator (withTimeGeneric)
import qualified Processor.RideEvents.Handlers as Handlers
import qualified Processor.RideEvents.Idempotency as Idem

processRideEnded :: RideEndedEvent -> Flow ()
processRideEnded event =
  withLogTag ("rideId-" <> event.rideId) $
    -- Event-level latency: total wall-clock to process one RideEndedEvent.
    -- producer_operation_duration{operation="rs-event:RideEndedEvent"} (seconds).
    void $
      withTimeGeneric "rs-event:RideEndedEvent" $ do
        runHandler "publishToAnalyticsKafka" event Handlers.handleAnalyticsKafka
        runHandler "publishRideInterpolation" event Handlers.handleRideInterpolation
        runHandler "computeAndStoreNammaTags" event Handlers.handleNammaTags
        runHandler "updateFleetAndOperatorStats" event Handlers.handleFleetOperatorStats
        runHandler "checkGpsTollBehavior" event Handlers.handleGpsTollBehavior
        runHandler "incrementRCStatsAndReminders" event Handlers.handleRCStatsReminders
        runHandler "notifyRideEnded" event Handlers.handleRideEndNotifications
        runHandler "updateLeaderboard" event Handlers.handleLeaderboard
        runHandler "sendReferralAndDriverToDriverReward" event Handlers.handleReferral

runHandler :: Text -> RideEndedEvent -> (RideEndedEvent -> Flow ()) -> Flow ()
runHandler name event handler =
  withLogTag name $
    Idem.withIdempotency name event.rideId $ do
      -- Per-handler latency: producer_operation_duration{operation="rs-handler:<name>"}
      -- (seconds). withTryCatch records the per-handler error counter on failure.
      (result, _ms) <- withTimeGeneric ("rs-handler:" <> name) (withTryCatch ("rs-handler:" <> name) (handler event))
      case result of
        Right () -> pure ()
        -- Re-throw so the entry stays pending and is retried by the transport,
        -- exactly as before. withTryCatch has already recorded the per-handler
        -- failure in try_exception_error_counter{ErrorContext="rs-handler:<name>"}.
        Left e -> throwM e
