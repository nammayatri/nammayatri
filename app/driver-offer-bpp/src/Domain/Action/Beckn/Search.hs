module Domain.Action.Beckn.Search where

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
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSearchReq
import Domain.Types.SearchRequestForDriver
import Environment
import Product.FareCalculator.Flow
import SharedLogic.DriverPool
import Storage.Queries.Person
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestForDriver as QSRD

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Maybe Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    gatewayUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity
  }

handler :: DOrg.Organization -> DSearchReq -> Flow ()
handler org sReq = do
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  searchReq <- buildSearchRequest fromLocation.id toLocation.id org.id sReq
  driverPool <- calculateDriverPool (getCoordinates fromLocation) org.id

  distance <-
    (.info.distance) <$> GoogleMaps.getDistance (Just MapSearch.CAR) (getCoordinates fromLocation) (getCoordinates toLocation) Nothing

  estimatedFare <- calculateFare org.id distance sReq.pickupTime
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated fare:"
      <> show estimatedFare
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver searchReq estimatedFare) driverPool
  Esq.runTransaction $ do
    QLoc.create fromLocation
    QLoc.create toLocation
    QSReq.create searchReq
    mapM_ QSRD.create searchRequestsForDrivers
  where
    buildSearchRequestForDriver ::
      (MonadFlow m) =>
      DSearchReq.SearchRequest ->
      FareParameters ->
      (DriverPoolResult, GoogleMaps.GetDistanceResultInfo) ->
      m SearchRequestForDriver
    buildSearchRequestForDriver searchRequest estFareParams (dpRes, gdRes) = do
      guid <- generateGUID
      now <- getCurrentTime
      pure
        SearchRequestForDriver
          { id = guid,
            searchRequestId = searchRequest.id,
            searchRequestValidTill = searchRequest.validTill,
            driverId = cast dpRes.driverId,
            vehicleVariant = dpRes.vehicle.variant,
            distanceToPickup = Meters $ floor gdRes.distance.getDistanceInMeter,
            durationToPickup = Seconds $ floor gdRes.duration,
            baseFare = fareSum estFareParams,
            createdAt = now,
            ..
          }

buildSearchRequest ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r Int
  ) =>
  Id DLoc.SearchReqLocation ->
  Id DLoc.SearchReqLocation ->
  Id DOrg.Organization ->
  DSearchReq ->
  m DSearchReq.SearchRequest
buildSearchRequest fromId toId orgId sReq = do
  id_ <- Id <$> generateGUID
  createdAt_ <- getCurrentTime
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = fromIntegral searchRequestExpirationSeconds `addUTCTime` createdAt_
  pure
    DSearchReq.SearchRequest
      { id = id_,
        transactionId = fromMaybe "" sReq.transactionId,
        messageId = sReq.messageId,
        validTill = validTill_,
        providerId = orgId,
        fromLocationId = fromId,
        toLocationId = toId,
        bapId = sReq.bapId,
        bapUri = sReq.bapUri,
        gatewayUri = sReq.gatewayUri,
        createdAt = createdAt_
      }

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}
