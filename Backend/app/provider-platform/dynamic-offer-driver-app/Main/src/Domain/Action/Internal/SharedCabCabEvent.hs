-- | rider-app's side of the cab's run: its silence check (no LTS ping for a while) pauses or ends the run, and a cab its riders have
-- all left takes the route queued for after the last drop. The answer is the update rider-app applies to its live copy; nothing here
-- calls rider-app back, so rider-app may call this while it waits on nothing of driver-app's.
module Domain.Action.Internal.SharedCabCabEvent where

import Domain.Types.SharedCabSession
import qualified Domain.Types.VehicleTrip as DVT
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Session as Session
import qualified Storage.CachedQueries.Merchant as QM

sharedCabCabEvent :: Text -> Maybe Text -> CabEventReq -> Flow CabEventResp
sharedCabCabEvent plate apiKey req = do
  -- an unknown plate, an unknown merchant and a wrong key all answer AuthBlocked
  mbSession <- Session.getSession plate
  mbMerchant <- maybe (pure Nothing) (QM.findById . (.merchantId)) mbSession
  case mbMerchant of
    Just m | Just m.internalApiKey == apiKey -> pure ()
    _ -> throwError $ AuthBlocked "Invalid BPP internal api key"
  update <- Session.withPlateLock plate $ do
    Session.readSession plate >>= \case
      Just s | s.status /= ENDED -> case req.event of
        CAB_SILENT_PAUSE
          | s.status == ACTIVE -> Just . Session.cabUpdate (Just CAB_PAUSED) False <$> Session.pause NO_LOCATION s
        CAB_OFF_ROUTE
          | s.status == ACTIVE -> Just . Session.cabUpdate (Just CAB_PAUSED) False <$> Session.pause OFF_ROUTE s
        CAB_SILENT_END -> Just . Session.cabUpdate (Just CAB_ENDED) False <$> Session.finishTrip DVT.SESSION_TIMEOUT s
        CAB_EMPTIED
          | Just queued <- s.queuedRouteCode -> Just . Session.cabUpdate (Just CAB_ROUTE_CHANGED) False <$> Session.switchTrip DVT.ROUTE_CHANGED queued s
        _ -> pure Nothing
      _ -> pure Nothing
  pure CabEventResp {update}
