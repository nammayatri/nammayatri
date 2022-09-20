module Domain.Action.Beckn.Select where

import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (getCoordinates))
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common (logDebug)
import qualified Data.Map as M
import Data.Time.Clock (addUTCTime)
import qualified Domain.Action.UI.GoogleMaps as GoogleMaps
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.SearchRequestForDriver
import Domain.Types.Vehicle.Variant (Variant)
import Environment
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import Storage.Queries.Person
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Maybe Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity,
    variant :: Variant
  }

type LanguageDictionary = M.Map GoogleMaps.Language DSearchReq.SearchRequest

handler :: Id DOrg.Organization -> DSelectReq -> Flow ()
handler orgId sReq = do
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  driverPool <- calculateDriverPool (Just sReq.variant) (getCoordinates fromLocation) orgId False

  distRes <- GoogleMaps.getDistance (Just MapSearch.CAR) (getCoordinates fromLocation) (getCoordinates toLocation) Nothing
  driverEstimatedPickupDuration <- asks (.driverEstimatedPickupDuration)
  let distance = distRes.distance
      estimatedRideDuration = distRes.duration_in_traffic
      estimatedRideFinishTime = realToFrac (driverEstimatedPickupDuration + estimatedRideDuration) `addUTCTime` sReq.pickupTime
  fareParams <- calculateFare orgId sReq.variant distance estimatedRideFinishTime Nothing
  searchReq <- buildSearchRequest fromLocation toLocation orgId sReq estimatedRideFinishTime
  let baseFare = fareSum fareParams
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated base fare:"
      <> show baseFare
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq baseFare distance) driverPool
  languageDictionary <- foldM (addLanguageToDictionary searchReq) M.empty driverPool
  Esq.runTransaction $ do
    QSReq.create searchReq
    mapM_ QSRD.create searchRequestsForDrivers
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) ->
    when (not dPoolRes.origin.onRide) $ do
      let language = fromMaybe GoogleMaps.ENGLISH dPoolRes.origin.language
      let translatedSearchReq = fromMaybe searchReq $ M.lookup language languageDictionary
      let entityData = makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq
      Notify.notifyOnNewSearchRequestAvailable sReqFD.driverId dPoolRes.origin.driverDeviceToken entityData
  where
    buildSearchRequestForDriver ::
      (MonadFlow m) =>
      DSearchReq.SearchRequest ->
      Money ->
      Meters ->
      GoogleMaps.GetDistanceResult DriverPoolResult MapSearch.LatLong ->
      m SearchRequestForDriver
    buildSearchRequestForDriver searchRequest baseFare_ distance gdRes = do
      guid <- generateGUID
      now <- getCurrentTime
      let driver = gdRes.origin
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                searchRequestId = searchRequest.id,
                startTime = searchRequest.startTime,
                searchRequestValidTill = searchRequest.validTill,
                driverId = cast driver.driverId,
                vehicleVariant = driver.vehicle.variant,
                distanceToPickup = gdRes.distance,
                durationToPickup = gdRes.duration,
                baseFare = baseFare_,
                createdAt = now,
                ..
              }
      pure searchRequestForDriver

buildSearchRequest ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Id DOrg.Organization ->
  DSelectReq ->
  UTCTime ->
  m DSearchReq.SearchRequest
buildSearchRequest from to orgId sReq estimatedRideFinishTime = do
  id_ <- Id <$> generateGUID
  createdAt_ <- getCurrentTime
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` createdAt_
  pure
    DSearchReq.SearchRequest
      { id = id_,
        transactionId = fromMaybe "" sReq.transactionId,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        estimatedFinishTime = estimatedRideFinishTime,
        validTill = validTill_,
        providerId = orgId,
        fromLocation = from,
        toLocation = to,
        bapId = sReq.bapId,
        bapUri = sReq.bapUri,
        createdAt = createdAt_,
        vehicleVariant = sReq.variant
      }

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

translateSearchReq ::
  ( MonadFlow m,
    GoogleMaps.HasGoogleMaps m r,
    CoreMetrics m
  ) =>
  DSearchReq.SearchRequest ->
  GoogleMaps.Language ->
  m DSearchReq.SearchRequest
translateSearchReq defaultSearchReq@DSearchReq.SearchRequest {..} language = do
  from <- buildLocWithAddr defaultSearchReq.fromLocation language
  to <- buildLocWithAddr defaultSearchReq.toLocation language
  pure
    DSearchReq.SearchRequest
      { fromLocation = from,
        toLocation = to,
        ..
      }

buildLocWithAddr ::
  ( MonadFlow m,
    GoogleMaps.HasGoogleMaps m r,
    CoreMetrics m
  ) =>
  DLoc.SearchReqLocation ->
  GoogleMaps.Language ->
  m DLoc.SearchReqLocation
buildLocWithAddr searchReqLoc@DLoc.SearchReqLocation {..} language = do
  placeNameResp <- GoogleMaps.getPlaceName (show searchReqLoc.lat <> "," <> show searchReqLoc.lon) (Just language)
  pure
    DLoc.SearchReqLocation
      { street = Nothing,
        door = Nothing,
        city = Nothing,
        state = Nothing,
        country = Nothing,
        building = Nothing,
        areaCode = Nothing,
        area = Just $ head $ placeNameResp.results <&> (.formatted_address),
        ..
      }

addLanguageToDictionary ::
  ( MonadFlow m,
    GoogleMaps.HasGoogleMaps m r,
    CoreMetrics m
  ) =>
  DSearchReq.SearchRequest ->
  LanguageDictionary ->
  (GoogleMaps.GetDistanceResult DriverPoolResult MapSearch.LatLong) ->
  m LanguageDictionary
addLanguageToDictionary searchReq dict dPoolRes = do
  let language = fromMaybe GoogleMaps.ENGLISH dPoolRes.origin.language
  if isJust $ M.lookup language dict
    then return dict
    else do
      translatedSearchReq <- translateSearchReq searchReq language
      pure $ M.insert language translatedSearchReq dict
