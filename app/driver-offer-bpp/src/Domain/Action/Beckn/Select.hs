module Domain.Action.Beckn.Select where

import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (getCoordinates))
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common (logDebug)
import Data.Time.Clock (addUTCTime)
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
  let baseFare = fareSumRounded fareParams
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated base fare:"
      <> show baseFare
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq baseFare distance) driverPool
  Esq.runTransaction $ do
    QSReq.create searchReq
    mapM_ QSRD.create searchRequestsForDrivers
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) ->
    when (not dPoolRes.origin.onRide) $ do
      let entityData = makeSearchRequestForDriverAPIEntity sReqFD searchReq
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
        createdAt = createdAt_
      }

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}
