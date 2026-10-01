-- | `05` §4 walk-up (board 8.4): scanning or typing a cab's plate resolves it to its live session and route, which
-- prefills the rider's search. The rider then books as usual and boards by typing the sticker code (8.1).
module SharedLogic.SharedCab.SpotBooking
  ( liveSharedCab,
    liveSharedCabByCode,
    CodeResolution (..),
    resolveCode,
    WalkUpChoice (..),
    cabRouteRequest,
    chooseWalkUp,
    isStickerCode,
    sharedCabVehicleData,
  )
where

import qualified API.Types.UI.MultimodalConfirm as ApiTypes
import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Char as Char
import Data.List (nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (..))
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error

-- | The ACTIVE shared-cab session a typed or scanned plate belongs to. Read-only: a rider's scan never rebuilds a
-- flushed session (the driver's poll and the expiry job do).
liveSharedCab :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
liveSharedCab plate = mfilter ((== ACTIVE) . (.status)) <$> Session.readSession (canonicalisePlate plate)

-- | The sticker code is the plate's last four digits (the same code boarding takes).
isStickerCode :: Text -> Bool
isStickerCode code = T.length code == 4 && T.all Char.isDigit code

data CodeResolution = NoCab | OneCab Text | ManyCabs
  deriving (Show, Eq)

-- | Which of the live plates a sticker code names: none, exactly one, or more than one (the rider then picks a route).
resolveCode :: Text -> [Text] -> CodeResolution
resolveCode code plates = case nub (filter ((== code) . T.takeEnd 4) plates) of
  [] -> NoCab
  [plate] -> OneCab plate
  _ -> ManyCabs

-- | Whether route serviceability is answered from the cabs: only when the shared-cab feed's route list could be read (Nothing
-- = no feed in this city, or the OTP call failed, so the unchanged bus path runs) and every requested route is one of its routes.
cabRouteRequest :: Maybe [Text] -> [Text] -> Bool
cabRouteRequest Nothing _ = False
cabRouteRequest (Just feedRouteCodes) requested = not (null requested) && all (`elem` feedRouteCodes) requested

data WalkUpChoice = UseBus | UseCab Text | PickRoute
  deriving (Show, Eq)

-- | A typed four-digit code is a bus first: when the bus lookup found a live vehicle for it the bus path keeps its meaning;
-- only otherwise a cab is tried (the bus path answers an unknown number with the whole feed, so "found" is decided from
-- the vehicle lookup, not from the bus path succeeding). Ambiguity applies among cabs only, and no cab leaves the bus
-- path's own answer for an unknown number.
chooseWalkUp :: Bool -> CodeResolution -> WalkUpChoice
chooseWalkUp busVehicleFound resolution
  | busVehicleFound = UseBus
  | otherwise = case resolution of
    OneCab plate -> UseCab plate
    ManyCabs -> PickRoute
    NoCab -> UseBus

-- | A typed sticker code resolved among the cabs live on the city's shared-cab routes (one route-set read per route,
-- the same index the route view uses), never the whole fleet.
liveSharedCabByCode :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow CodeResolution
liveSharedCabByCode integratedBppConfig code = do
  routes <- OTPRest.getRoutesByGtfsId integratedBppConfig
  plates <- concatMap (map (.vehicleNumber)) <$> mapM (Session.activeSessionsOnRoute . (.code)) routes
  pure (resolveCode code plates)

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
