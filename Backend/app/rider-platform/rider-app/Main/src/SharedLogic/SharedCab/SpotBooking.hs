-- | `05` §4 walk-up (board 8.4): scanning or typing a cab's plate resolves it to its live session and route, which
-- prefills the rider's search. The rider then books as usual and boards by typing the sticker code (8.1).
module SharedLogic.SharedCab.SpotBooking
  ( liveSharedCab,
    sharedCabVehicleData,
  )
where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified BecknV2.FRFS.Enums as Spec
import Data.List (sortOn)
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error

-- | The ACTIVE shared-cab session a typed or scanned plate belongs to.
liveSharedCab :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
liveSharedCab plate = mfilter ((== ACTIVE) . (.status)) <$> Session.getSession plate

-- | The cab's current route as vehicle data, the shape the bus path returns, with the tier set to SHARED_CAB.
sharedCabVehicleData :: Session -> Environment.Flow ApiTypes.PublicTransportData
sharedCabVehicleData s = do
  integratedBppConfig <- CQIBC.findById s.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound
  mbRoute <- OTPRest.getRouteByRouteId integratedBppConfig s.routeCode
  stops <- sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode s.routeCode integratedBppConfig
  gtfsVersion <- either (const integratedBppConfig.feedKey) identity <$> withTryCatch "sharedCabVehicleData:gtfsVersion" (OTPRest.getGtfsVersion integratedBppConfig)
  let ibc = integratedBppConfig.id
      vehicleType = maybe (show Spec.BUS) (show . (.vehicleType)) mbRoute
  pure
    ApiTypes.PublicTransportData
      { rs =
          [ ApiTypes.TransportRoute
              { cd = s.routeCode,
                sN = maybe s.routeCode (.shortName) mbRoute,
                lN = maybe s.routeCode (.longName) mbRoute,
                dTC = Nothing,
                stC = Just (length stops),
                vt = vehicleType,
                st = Just Spec.SHARED_CAB,
                stn = Nothing,
                sst = Nothing,
                clr = mbRoute >>= (.color),
                tid = Nothing,
                ibc
              }
          ],
        ss =
          [ ApiTypes.TransportStation
              { cd = stop.stopCode,
                nm = stop.stopName,
                lt = stop.stopPoint.lat,
                ln = stop.stopPoint.lon,
                vt = vehicleType,
                ad = Nothing,
                rgn = Nothing,
                hin = Nothing,
                sgstdDest = Nothing,
                gj = Nothing,
                gi = Nothing,
                ibc,
                lty = Nothing,
                psc = Nothing,
                pf = Nothing
              }
            | stop <- stops
          ],
        rsm = [ApiTypes.TransportRouteStopMapping {rc = stop.routeCode, sc = stop.stopCode, sn = stop.sequenceNum, ibc} | stop <- stops],
        ptcv = gtfsVersion,
        eligiblePassIds = Nothing,
        isHistoric = False,
        scheduleBasedActiveTrip = False,
        waybillStatus = Nothing
      }
