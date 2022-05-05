module Domain.Action.Beckn.Search where

import App.Types
import Beckn.Serviceability
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Data.List as List
import Data.Traversable
import qualified Domain.Action.Beckn.Search.OneWay as OneWay
import qualified Domain.Action.Beckn.Search.Rental as Rental
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSearchRequest
import EulerHS.Prelude hiding (id, state)
import Product.Location
import qualified Storage.Queries.FarePolicy.FareProduct as QFareProduct
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Tools.Metrics as Metrics
import Types.Error
import Utils.Common

data DSearchReq = DSearchReq
  { transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    mbDropLocation :: Maybe DLoc.SearchReqLocationAPIEntity
  }

data DOnSearchReq = DOnSearchReq
  { transporterInfo :: TransporterInfo,
    quotesInfo :: QuotesInfo
  }

data QuotesInfo = OneWay [OneWay.QuoteInfo] | Rental [Rental.QuoteInfo]

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DOrg.Organization,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

findTransporter :: Id DOrg.Organization -> Flow DOrg.Organization
findTransporter transporterId = do
  transporter <- QOrg.findById transporterId >>= fromMaybeM OrgDoesNotExist
  unless transporter.enabled $ throwError AgencyDisabled
  pure transporter

handler :: DOrg.Organization -> DSearchReq -> Flow DOnSearchReq
handler transporter req@DSearchReq {..} = do
  let pickupLatLong = locationToLatLong pickupLocation
  let mbDropoffLatLong = locationToLatLong <$> mbDropLocation
  unlessM (rideServiceable QGeometry.someGeometriesContain pickupLatLong mbDropoffLatLong) $
    throwError RideNotServiceable
  whenJustM
    (QSearchRequest.findByTxnIdAndBapIdAndBppId transactionId bapId transporter.id)
    (\_ -> throwError $ InvalidRequest "Duplicate Search request")

  searchMetricsMVar <- Metrics.startSearchMetrics transporter.id

  now <- getCurrentTime
  validity <- getValidTime now pickupTime
  fromLocation <- buildSearchReqLoc now pickupLocation
  mbToLocation <- buildSearchReqLoc now `traverse` mbDropLocation
  searchRequest <- buildSearchRequest req transporter.id now validity fromLocation.id (mbToLocation <&> (.id))
  Esq.runTransaction $ do
    QLoc.create fromLocation
    whenJust mbToLocation QLoc.create
    QSearchRequest.create searchRequest

  fareProducts <- QFareProduct.findEnabledByOrgId transporter.id
  let isRentalProduct = any (\fareProduct -> fareProduct._type == DFareProduct.RENTAL) fareProducts
  let isOneWayProduct = any (\fareProduct -> fareProduct._type == DFareProduct.ONE_WAY) fareProducts
  quotesInfo <-
    case mbToLocation of
      Nothing -> do
        if isRentalProduct
          then Rental <$> Rental.onSearchCallback searchRequest.id transporter.id now
          else pure $ Rental []
      Just toLocation -> do
        if isOneWayProduct
          then OneWay <$> OneWay.onSearchCallback searchRequest transporter.id now fromLocation toLocation
          else pure $ OneWay []
  buildOnSearchReq transporter quotesInfo
    <* Metrics.finishSearchMetrics transporter.id searchMetricsMVar

buildSearchReqLoc ::
  MonadGuid m =>
  UTCTime ->
  DLoc.SearchReqLocationAPIEntity ->
  m DLoc.SearchReqLocation
buildSearchReqLoc now DLoc.SearchReqLocationAPIEntity {..} = do
  locId <- generateGUID
  return
    DLoc.SearchReqLocation
      { id = locId,
        createdAt = now,
        updatedAt = now,
        ..
      }

getValidTime :: HasFlowEnv m r '["caseExpiry" ::: Maybe Seconds] => UTCTime -> UTCTime -> m UTCTime
getValidTime now startTime = do
  caseExpiry_ <- maybe 7200 fromIntegral <$> asks (.caseExpiry)
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

buildSearchRequest ::
  MonadGuid m =>
  DSearchReq ->
  Id DOrg.Organization ->
  UTCTime ->
  UTCTime ->
  Id DLoc.SearchReqLocation ->
  Maybe (Id DLoc.SearchReqLocation) ->
  m DSearchRequest.SearchRequest
buildSearchRequest DSearchReq {..} transporterId now validity fromLocationId mbToLocationId = do
  uuid <- generateGUID
  pure
    DSearchRequest.SearchRequest
      { id = Id uuid,
        transactionId = transactionId,
        startTime = pickupTime,
        validTill = validity,
        providerId = transporterId,
        fromLocationId = fromLocationId,
        toLocationId = mbToLocationId,
        bapId = bapId,
        bapUri = bapUri,
        createdAt = now
      }

buildOnSearchReq ::
  EsqDBFlow m r =>
  DOrg.Organization ->
  QuotesInfo ->
  m DOnSearchReq
buildOnSearchReq org quotesInfo = do
  count <- QRide.getCountByStatus org.id
  let transporterInfo =
        TransporterInfo
          { shortId = org.shortId,
            name = org.name,
            contacts = fromMaybe "" org.mobileNumber,
            ridesInProgress = fromMaybe 0 $ List.lookup DRide.INPROGRESS count,
            ridesCompleted = fromMaybe 0 $ List.lookup DRide.COMPLETED count,
            ridesConfirmed = fromMaybe 0 $ List.lookup DRide.NEW count
          }
  pure $ DOnSearchReq {transporterInfo, quotesInfo}
