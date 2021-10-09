{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.Search (search) where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Search as API
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Tag as Tag
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Mobility.Stop as Stop
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Traversable
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified ExternalAPI.Transform as ExternalAPITransform
import qualified Product.BecknProvider.BP as BP
import Product.FareCalculator
import qualified Product.Location as Loc
import qualified Product.Person as Person
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.ProductInstance as PI
import qualified Storage.Queries.Products as SProduct
import qualified Storage.Queries.SearchReqLocation as Loc
import qualified Test.RandomStrings as RS
import qualified Types.API.Case as APICase
import Types.Error
import Types.Metrics (CoreMetrics, HasBPPMetrics)
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.Vehicle as Vehicle
import Utils.Common
import qualified Utils.Metrics as Metrics

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
        let productCase = mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId
        DB.runSqlDBTransaction $ do
          Loc.create fromLocation
          Loc.create toLocation
          QCase.create productCase
        ExternalAPI.withCallback' withRetry transporter "search" API.onSearch context callbackUrl $
          onSearchCallback productCase transporter fromLocation toLocation searchMetricsMVar

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

mkCase :: API.SearchReq -> Text -> UTCTime -> UTCTime -> UTCTime -> Location.SearchReqLocation -> Location.SearchReqLocation -> Id Org.Organization -> Id Org.Organization -> Case.Case
mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId = do
  let intent = req.message.intent
  let distance = Tag.value <$> find (\x -> x.key == "distance") (fromMaybe [] $ intent.tags)
  let tId = getId transporterId
  let bapId = getId bapOrgId
  Case.Case
    { id = Id uuid,
      name = Nothing,
      description = Just "Case to search for a Ride",
      shortId = ShortId $ tId <> "_" <> req.context.transaction_id,
      industry = Case.MOBILITY,
      _type = Case.RIDESEARCH,
      exchangeType = Case.FULFILLMENT,
      status = Case.NEW,
      startTime = startTime,
      endTime = Nothing,
      validTill = validity,
      provider = Just tId,
      providerType = Nothing,
      requestor = Nothing,
      requestorType = Just Case.CONSUMER,
      parentCaseId = Nothing,
      fromLocationId = fromLocation.id,
      toLocationId = toLocation.id,
      udf1 = intent.vehicle <&> (.variant),
      udf2 = Just $ show $ length $ intent.payload.travellers,
      udf3 = Nothing,
      udf4 = Just bapId,
      udf5 = distance,
      info = Nothing, --Just $ show $ req.message
      createdAt = now,
      updatedAt = now
    }

onSearchCallback ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  Case.Case ->
  Org.Organization ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  Metrics.SearchMetricsMVar ->
  m API.OnSearchServices
onSearchCallback productCase transporter fromLocation toLocation searchMetricsMVar = do
  let transporterId = transporter.id
  let desiredVehicleVariant = productCase.udf1 >>= readMaybe . T.unpack
  pool <- Person.calculateDriverPool (fromLocation.id) transporterId desiredVehicleVariant
  logTagInfo "OnSearchCallback" $
    "Calculated Driver Pool for organization " +|| getId transporterId
      ||+ " with drivers " +| T.intercalate ", " (getId . (.driverId) <$> pool) |+ ""

  let listOfProtoPIs =
        catMaybes $
          everyPossibleVariant <&> \var ->
            find ((== var) . (.variant)) pool
  -- drivers sorted from nearest to furthest, so with `find`
  -- we take nearest one and calculate fare and make PI for him

  distance <-
    map Loc.locationToLatLong [fromLocation, toLocation]
      & MapSearch.getDistanceMb (Just MapSearch.CAR)
      >>= fromMaybeM CantCalculateDistance

  listOfPIs <-
    for listOfProtoPIs $ \poolResult -> do
      price <- calculateFare transporterId poolResult.variant distance productCase.startTime
      mkProductInstance productCase price PI.INSTOCK transporterId poolResult.distanceToDriver poolResult.variant

  DB.runSqlDBTransaction $ do
    for_ listOfPIs PI.create
    let newCaseStatus = if null listOfPIs then Case.CLOSED else Case.CONFIRMED
    QCase.updateStatus productCase.id newCaseStatus

  mkOnSearchPayload productCase listOfPIs transporter
    <* Metrics.finishSearchMetrics transporterId searchMetricsMVar

mkProductInstance ::
  DBFlow m r =>
  Case.Case ->
  Amount ->
  PI.ProductInstanceStatus ->
  Id Org.Organization ->
  Double ->
  Vehicle.Variant ->
  m PI.ProductInstance
mkProductInstance productCase price status transporterId nearestDriverDist vehicleVariant = do
  productInstanceId <- Id <$> L.generateGUID
  now <- getCurrentTime
  shortId <- L.runIO $ T.pack <$> RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  products <-
    SProduct.findByName (show vehicleVariant)
      >>= fromMaybeM ProductsNotFound
  return
    PI.ProductInstance
      { id = productInstanceId,
        caseId = productCase.id,
        productId = products.id,
        personId = Nothing,
        personUpdatedAt = Nothing,
        shortId = ShortId shortId,
        entityType = PI.VEHICLE,
        entityId = Nothing,
        quantity = 1,
        _type = Case.RIDESEARCH,
        price,
        actualPrice = Nothing,
        status = status,
        startTime = productCase.startTime,
        endTime = productCase.endTime,
        validTill = productCase.validTill,
        fromLocation = Just $ productCase.fromLocationId,
        toLocation = Just $ productCase.toLocationId,
        organizationId = transporterId,
        parentId = Nothing,
        traveledDistance = 0,
        chargeableDistance = Nothing,
        vehicleVariant,
        udf1 = Just $ show nearestDriverDist,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        info = productCase.info,
        createdAt = now,
        updatedAt = now
      }

mkOnSearchPayload ::
  DBFlow m r =>
  Case.Case ->
  [PI.ProductInstance] ->
  Org.Organization ->
  m API.OnSearchServices
mkOnSearchPayload productCase productInstances transporterOrg = do
  PI.getCountRideOrder (transporterOrg.id)
    <&> mkProviderInfo transporterOrg . mkProviderStats
    >>= ExternalAPITransform.mkCatalog productCase productInstances
    <&> API.OnSearchServices

mkProviderInfo :: Org.Organization -> APICase.ProviderStats -> APICase.ProviderInfo
mkProviderInfo org stats =
  APICase.ProviderInfo
    { id = getId $ org.id,
      name = org.name,
      stats = encodeToText stats,
      contacts = fromMaybe "" (org.mobileNumber)
    }

mkProviderStats :: [(PI.ProductInstanceStatus, Int)] -> APICase.ProviderStats
mkProviderStats piCount =
  APICase.ProviderStats
    { completed = List.lookup PI.COMPLETED piCount,
      inprogress = List.lookup PI.INPROGRESS piCount,
      confirmed = List.lookup PI.CONFIRMED piCount
    }
