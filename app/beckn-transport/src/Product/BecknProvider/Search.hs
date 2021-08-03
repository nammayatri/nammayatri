{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.Search (search) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Search as API
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Tag as Tag
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Stop as Stop
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified ExternalAPI.Transform as ExternalAPITransform
import qualified Product.BecknProvider.BP as BP
import qualified Product.BecknProvider.Confirm as Confirm
import Product.FareCalculator
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.Products as SProduct
import qualified Storage.Queries.SearchReqLocation as Loc
import qualified Storage.Queries.Quote as Quote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Test.RandomStrings as RS
import qualified Types.Common as Common
import Types.Error
import Types.Metrics (CoreMetrics, HasBPPMetrics)
import qualified Types.Storage.OldRide as Ride
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Vehicle as Veh
import Utils.Common
import qualified Utils.Metrics as Metrics
import Servant.Client (showBaseUrl)

search ::
  Id Org.Organization ->
  SignatureAuthResult Org.Organization ->
  SignatureAuthResult Org.Organization ->
  API.SearchReq ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult _ bapOrg) (SignatureAuthResult _ _gateway) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    let context = req.context
    BP.validateContext "search" context
    transporter <-
      Org.findOrganizationById transporterId
        >>= fromMaybeM OrgDoesNotExist
    callbackUrl <- ExternalAPI.getGatewayUrl
    if not transporter.enabled
      then
        ExternalAPI.withCallback' withRetry transporter "search" API.onSearch context callbackUrl $
          throwError AgencyDisabled
      else do
        searchMetricsMVar <- Metrics.startSearchMetrics transporterId
        let intent = req.message.intent
        now <- getCurrentTime
        let pickup = head $ intent.pickups
        let dropOff = head $ intent.drops
        let startTime = pickup.departure_time.est
        validity <- getValidTime now startTime
        fromLocation <- buildFromStop now pickup
        toLocation <- buildFromStop now dropOff
        let bapOrgId = bapOrg.id
        uuid <- L.generateGUID
        vehVariant <- readMaybe (T.unpack req.message.intent.vehicle.variant) & fromMaybeM (InvalidRequest "Unable to parse vehicle variant.")
        bapUri <- req.context.bap_uri & fromMaybeM (InvalidRequest "Context must have bap_uri")
        let searchRequest = mkSearchRequest req uuid now validity startTime fromLocation toLocation transporterId bapOrgId vehVariant bapUri
        let distance = Tag.value <$> find (\x -> x.key == "distance") (fromMaybe [] $ intent.tags)
        DB.runSqlDBTransaction $ do
          Loc.create fromLocation
          Loc.create toLocation
          QSearchRequest.create searchRequest
        ExternalAPI.withCallback' withRetry transporter "search" API.onSearch context callbackUrl $
          onSearchCallback searchRequest transporter fromLocation toLocation searchMetricsMVar distance

buildFromStop :: MonadFlow m => UTCTime -> Stop.Stop -> m Location.SearchReqLocation
buildFromStop now stop = do
  let loc = stop.location
  let mgps = loc.gps
  let maddress = loc.address
  uuid <- Id <$> L.generateGUID
  lat <- mgps >>= readMaybe . T.unpack . (.lat) & fromMaybeM (InvalidRequest "Lat field is not present.")
  lon <- mgps >>= readMaybe . T.unpack . (.lon) & fromMaybeM (InvalidRequest "Lon field is not present.")
  pure $
    Location.SearchReqLocation
      { id = uuid,
        lat = lat,
        long = lon,
        district = Nothing,
        city = (^. #city) <$> maddress,
        state = (^. #state) <$> maddress,
        country = (^. #country) <$> maddress,
        pincode = (^. #area_code) <$> maddress,
        address = encodeToText <$> maddress,
        createdAt = now,
        updatedAt = now
      }

getValidTime :: HasFlowEnv m r '["caseExpiry" ::: Maybe Seconds] => UTCTime -> UTCTime -> m UTCTime
getValidTime now startTime = do
  caseExpiry_ <- maybe 7200 fromIntegral <$> asks (.caseExpiry)
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

mkSearchRequest ::
  API.SearchReq ->
  Text ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  Id Org.Organization ->
  Id Org.Organization ->
  Veh.Variant ->
  BaseUrl ->
  SearchRequest.SearchRequest
mkSearchRequest req uuid now validity startTime fromLocation toLocation transporterId bapOrgId vehVariant bapUri = do
  let bapId = getId bapOrgId
  SearchRequest.SearchRequest
    { id = Id uuid,
      transactionId = req.context.transaction_id,
      startTime = startTime,
      validTill = validity,
      providerId = transporterId,
      requestorId = Id "", -- TODO: Fill this field with BAPPerson id.
      fromLocationId = fromLocation.id,
      toLocationId = toLocation.id,
      vehicleVariant = vehVariant,
      bapId = bapId,
      bapUri = T.pack $ showBaseUrl bapUri,
      createdAt = now
    }

onSearchCallback ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  SearchRequest.SearchRequest ->
  Org.Organization ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  Metrics.SearchMetricsMVar ->
  Maybe Text ->
  m API.OnSearchServices
onSearchCallback searchRequest transporter fromLocation toLocation searchMetricsMVar distance = do
  let transporterId = transporter.id
      vehicleVariant = searchRequest.vehicleVariant
  pool <- Confirm.calculateDriverPool (fromLocation.id) transporterId vehicleVariant
  logTagInfo "OnSearchCallback" $
    "Calculated Driver Pool for organization " +|| getId transporterId ||+ " with drivers " +| T.intercalate ", " (getId . fst <$> pool) |+ ""
  let quoteStatus =
        if null pool
          then Quote.OUTOFSTOCK
          else Quote.INSTOCK
  (price, nearestDriverDist) <-
    case pool of
      [] -> return (Nothing, Nothing)
      (fstDriverValue : _) -> do
        let dstSrc = maybe (Left (fromLocation, toLocation)) Right (distance >>= readMaybe . T.unpack)
        fare <- Just <$> calculateFare transporterId vehicleVariant dstSrc (searchRequest.startTime)
        let nearestDist = Just $ snd fstDriverValue
        return (fare, nearestDist)
  quote <- mkQuote searchRequest price quoteStatus transporterId nearestDriverDist
  DB.runSqlDBTransaction $ do
    Quote.create quote
  let quotes =
        case quote.status of
          Quote.OUTOFSTOCK -> []
          _ -> [quote]
  res <- mkOnSearchPayload searchRequest quotes transporter
  Metrics.finishSearchMetrics transporterId searchMetricsMVar
  return res

mkQuote ::
  DBFlow m r =>
  SearchRequest.SearchRequest ->
  Maybe Amount ->
  Quote.QuoteStatus ->
  Id Org.Organization ->
  Maybe Double ->
  m Quote.Quote
mkQuote productSearchRequest price status transporterId nearestDriverDist = do
  quoteId <- Id <$> L.generateGUID
  now <- getCurrentTime
  shortId <- L.runIO $ T.pack <$> RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  products <-
    SProduct.findByName (show productSearchRequest.vehicleVariant)
      >>= fromMaybeM ProductsNotFound
  return
    Quote.Quote
      { id = quoteId,
        requestId = productSearchRequest.id,
        productId = products.id,
        personId = Nothing,
        personUpdatedAt = Nothing,
        shortId = ShortId shortId,
        entityType = Quote.VEHICLE,
        entityId = Nothing,
        quantity = 1,
        price = price,
        actualPrice = Nothing,
        status = status,
        startTime = productSearchRequest.startTime,
        endTime = Nothing,
        validTill = productSearchRequest.validTill,
        fromLocation = Just $ productSearchRequest.fromLocationId,
        toLocation = Just $ productSearchRequest.toLocationId,
        organizationId = transporterId,
        distance = 0,
        udf1 = show <$> nearestDriverDist,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        info = Nothing,
        createdAt = now,
        updatedAt = now
      }

mkOnSearchPayload ::
  DBFlow m r =>
  SearchRequest.SearchRequest ->
  [Quote.Quote] ->
  Org.Organization ->
  m API.OnSearchServices
mkOnSearchPayload productSearchRequest quotes transporterOrg = do
  QRide.getCountByStatus (transporterOrg.id)
    <&> mkProviderInfo transporterOrg . mkProviderStats
    >>= ExternalAPITransform.mkCatalog productSearchRequest quotes
    <&> API.OnSearchServices

mkProviderInfo :: Org.Organization -> Common.ProviderStats -> Common.ProviderInfo
mkProviderInfo org stats =
  Common.ProviderInfo
    { id = getId $ org.id,
      name = org.name,
      stats = encodeToText stats,
      contacts = fromMaybe "" (org.mobileNumber)
    }

mkProviderStats :: [(Ride.RideStatus, Int)] -> Common.ProviderStats
mkProviderStats piCount =
  Common.ProviderStats
    { completed = List.lookup Ride.COMPLETED piCount,
      inprogress = List.lookup Ride.INPROGRESS piCount,
      confirmed = List.lookup Ride.CONFIRMED piCount
    }
