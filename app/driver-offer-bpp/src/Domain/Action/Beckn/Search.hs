module Domain.Action.Beckn.Search where

import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (getCoordinates))
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnSearch as API
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common
import Core.ACL.OnSearch
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant as Variant
import Environment
import ExternalAPI.Flow (withCallback')
import Product.FareCalculator.Flow
import SharedLogic.DriverPool
import qualified Storage.Queries.SearchRequest as QSReq
import Types.Error
import Utils.Context (contextTemplate)

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
  searchReq <- buildSearchRequest fromLocation toLocation org.id sReq
  driverPool <- calculateDriverPool (getCoordinates fromLocation) org.id
  distanceToPickup <-
    case driverPool of
      [] -> throwError (InternalError "No drivers available for this ride")
      (x : _) -> pure x.distance

  distance <-
    metersToHighPrecMeters . (.distance)
      <$> GoogleMaps.getDistance (Just MapSearch.CAR) (getCoordinates fromLocation) (getCoordinates toLocation) Nothing

  estimatedFare <- amountToDouble . fareSum <$> calculateFare org.id distance sReq.pickupTime
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated fare:"
      <> show estimatedFare
  Esq.runTransaction $ do
    QSReq.create searchReq
  context <- contextTemplate org Context.SEARCH sReq.bapId sReq.bapUri sReq.transactionId sReq.messageId
  logDebug $ "bap uri: " <> show sReq.bapUri
  let callbackUrl = sReq.gatewayUri
      variant = Variant.AUTO
      action = buildOnSearchReq org variant distanceToPickup estimatedFare searchReq <&> mkOnSearchMessage
  void $ withCallback' withRetry org Context.SEARCH API.onSearchAPI context callbackUrl action

buildSearchRequest ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r Int
  ) =>
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Id DOrg.Organization ->
  DSearchReq ->
  m DSearchReq.SearchRequest
buildSearchRequest from to orgId sReq = do
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

buildOnSearchReq ::
  (MonadTime m) =>
  DOrg.Organization ->
  Variant.Variant ->
  Meters ->
  Double ->
  DSearchReq.SearchRequest ->
  m DOnSearchReq
buildOnSearchReq org vehicleVariant distanceToPickup baseFare searchRequest = do
  now <- getCurrentTime
  let transporterInfo =
        TransporterInfo
          { shortId = org.shortId,
            name = org.name,
            contacts = fromMaybe "" org.mobileNumber,
            ridesInProgress = 0, -- FIXME
            ridesCompleted = 0, -- FIXME
            ridesConfirmed = 0 -- FIXME
          }
  pure $
    DOnSearchReq
      { transporterInfo,
        now,
        searchRequest,
        vehicleVariant,
        distanceToPickup,
        baseFare
      }
