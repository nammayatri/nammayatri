module Domain.Action.Beckn.Search where

import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (getCoordinates))
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
--import qualified Beckn.Types.MapSearch as MapSearch

import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common
import Data.List
import Domain.Types.BusinessEvent (WhenPoolWasComputed (ON_SEARCH))
import qualified Domain.Types.Organization as DOrg
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant as Variant
import Environment
import Product.FareCalculator.Flow
import SharedLogic.DriverPool
import qualified Storage.Queries.BusinessEvent as QBE
import Storage.Queries.Person (DriverPoolResult)
import qualified Storage.Queries.SearchRequest as QSReq

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Maybe Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity
  }

data DSearchRes = DSearchRes
  { transporterInfo :: TransporterInfo,
    searchRequest :: SearchRequest,
    now :: UTCTime,
    estimateList :: [EstimateItem]
  }

data EstimateItem = EstimateItem
  { vehicleVariant :: Variant.Variant,
    distanceToPickup :: Meters,
    baseFare :: Amount
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DOrg.Organization,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

handler :: DOrg.Organization -> DSearchReq -> Flow DSearchRes
handler org sReq = do
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  driverPool <- calculateDriverPool Nothing (getCoordinates fromLocation) org.id
  let getVariant x = x.origin.vehicle.variant
      listOfProtoQuotes = nubBy ((==) `on` getVariant) driverPool

  distance <-
    metersToHighPrecMeters . (.distance)
      <$> GoogleMaps.getDistance (Just MapSearch.CAR) (getCoordinates fromLocation) (getCoordinates toLocation) Nothing

  estimates <- mapM (mkEstimate org sReq distance) listOfProtoQuotes
  searchReq <- buildSearchRequest fromLocation toLocation org.id sReq
  Esq.runTransaction $ do
    QSReq.create searchReq
    traverse_ (QBE.logDriverInPoolEvent ON_SEARCH Nothing) driverPool
  logDebug $ "bap uri: " <> show sReq.bapUri
  buildSearchRes org searchReq estimates

mkEstimate ::
  (MonadFlow m, Esq.Transactionable m) =>
  DOrg.Organization ->
  DSearchReq ->
  HighPrecMeters ->
  GoogleMaps.GetDistanceResult DriverPoolResult a ->
  m EstimateItem
mkEstimate org dSReq dist g = do
  let variant = g.origin.vehicle.variant
  fareParams <- calculateFare org.id variant dist dSReq.pickupTime Nothing
  let baseFare = fareSum fareParams
  logDebug $ "baseFare: " <> show (amountToString baseFare)
  logDebug $ "distance: " <> show g.distance
  pure
    EstimateItem
      { vehicleVariant = g.origin.vehicle.variant,
        distanceToPickup = g.distance,
        baseFare
      }

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
        startTime = sReq.pickupTime,
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

buildSearchRes ::
  (MonadTime m) =>
  DOrg.Organization ->
  DSearchReq.SearchRequest ->
  [EstimateItem] ->
  m DSearchRes
buildSearchRes org searchRequest estimateList = do
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
    DSearchRes
      { transporterInfo,
        now,
        searchRequest,
        estimateList
      }
