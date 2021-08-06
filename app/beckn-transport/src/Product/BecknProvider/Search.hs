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
import Product.FareCalculator
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Location as Loc
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as SProduct
import qualified Test.RandomStrings as RS
import qualified Types.API.Case as APICase
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Location as Location
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.ProductInstance as ProductInstance
import Utils.Common

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
    unless (transporter.enabled) $ throwError ServiceUnavailable
    let intent = req.message.intent
    now <- getCurrentTime
    let pickup = head $ intent.pickups
    let dropOff = head $ intent.drops
    let startTime = pickup.departure_time.est
    validity <- getValidTime now startTime
    fromLocation <- mkFromStop now pickup
    toLocation <- mkFromStop now dropOff
    let bapOrgId = bapOrg.id
    deadDistance <- calculateDeadDistance transporter fromLocation
    uuid <- L.generateGUID
    let productCase = mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance
    DB.runSqlDBTransaction $ do
      Loc.create fromLocation
      Loc.create toLocation
      QCase.create productCase
    callbackUrl <- ExternalAPI.getGatewayUrl
    ExternalAPI.withCallback' withRetry transporter "search" API.onSearch context callbackUrl $
      onSearchCallback productCase transporter fromLocation toLocation

mkFromStop :: MonadFlow m => UTCTime -> Stop.Stop -> m Location.Location
mkFromStop now stop = do
  let loc = stop.location
  let mgps = loc.gps
  let maddress = loc.address
  uuid <- Id <$> L.generateGUID
  pure $
    Location.Location
      { id = uuid,
        locationType = Location.POINT,
        lat = read . T.unpack . (^. #lat) <$> mgps,
        long = read . T.unpack . (^. #lon) <$> mgps,
        ward = (^. #ward) =<< maddress,
        district = Nothing,
        city = (^. #city) <$> maddress,
        state = (^. #state) <$> maddress,
        country = (^. #country) <$> maddress,
        pincode = (^. #area_code) <$> maddress,
        address = encodeToText <$> maddress,
        bound = Nothing,
        point = Location.Point,
        createdAt = now,
        updatedAt = now
      }

getValidTime :: HasFlowEnv m r '["caseExpiry" ::: Maybe Second] => UTCTime -> UTCTime -> m UTCTime
getValidTime now startTime = do
  caseExpiry_ <- maybe 7200 fromIntegral <$> asks (.caseExpiry)
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

mkCase :: API.SearchReq -> Text -> UTCTime -> UTCTime -> UTCTime -> Location.Location -> Location.Location -> Id Org.Organization -> Id Org.Organization -> Maybe Double -> Case.Case
mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance = do
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
      udf1 = Just $ intent.vehicle.variant,
      udf2 = Just $ show $ length $ intent.payload.travellers,
      udf3 = encodeToText <$> deadDistance,
      udf4 = Just bapId,
      udf5 = distance,
      info = Nothing, --Just $ show $ req.message
      createdAt = now,
      updatedAt = now
    }

calculateDeadDistance ::
  ( DBFlow m r,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  Location.Location ->
  m (Maybe Double)
calculateDeadDistance organization fromLocation = do
  eres <- try $ do
    orgLocId <- organization.locationId & fromMaybeM (OrgFieldNotPresent "location_id")
    mbOrgLocation <- Loc.findLocationById orgLocId
    case mbOrgLocation of
      Nothing -> throwError LocationNotFound
      Just orgLocation -> Location.calculateDistance orgLocation fromLocation
  case eres of
    Left (err :: SomeException) -> do
      logTagWarning "calculateDeadDistance" $ "Failed to calculate distance. Reason: " +|| err ||+ ""
      pure Nothing
    Right mDistance -> return mDistance

onSearchCallback ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meter],
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Case.Case ->
  Org.Organization ->
  Location.Location ->
  Location.Location ->
  m API.OnSearchServices
onSearchCallback productCase transporter fromLocation toLocation = do
  let transporterId = transporter.id
  vehicleVariant <-
    (productCase.udf1 >>= readMaybe . T.unpack)
      & fromMaybeM (CaseFieldNotPresent "udf1")
  pool <- Person.calculateDriverPool (fromLocation.id) transporterId vehicleVariant
  logTagInfo "OnSearchCallback" $
    "Calculated Driver Pool for organization " +|| getId transporterId ||+ " with drivers " +| T.intercalate ", " (getId . fst <$> pool) |+ ""
  let piStatus =
        if null pool
          then ProductInstance.OUTOFSTOCK
          else ProductInstance.INSTOCK
  (price, nearestDriverDist) <-
    case pool of
      [] -> return (Nothing, Nothing)
      (fstDriverValue : _) -> do
        fare <- Just <$> calculateFare transporterId vehicleVariant fromLocation toLocation (productCase.startTime) (productCase.udf5)
        let nearestDist = Just $ snd fstDriverValue
        return (fare, nearestDist)
  prodInst <- mkProductInstance productCase price piStatus transporterId nearestDriverDist
  let caseStatus ProductInstance.INSTOCK = Case.CONFIRMED
      caseStatus _ = Case.CLOSED
  DB.runSqlDBTransaction $ do
    ProductInstance.create prodInst
    QCase.updateStatus (productCase.id) (caseStatus $ prodInst.status)
  let productInstances =
        case prodInst.status of
          ProductInstance.OUTOFSTOCK -> []
          _ -> [prodInst]
  mkOnSearchPayload productCase productInstances transporter

mkProductInstance ::
  DBFlow m r =>
  Case.Case ->
  Maybe Amount ->
  ProductInstance.ProductInstanceStatus ->
  Id Org.Organization ->
  Maybe Double ->
  m ProductInstance.ProductInstance
mkProductInstance productCase price status transporterId nearestDriverDist = do
  productInstanceId <- Id <$> L.generateGUID
  now <- getCurrentTime
  shortId <- L.runIO $ T.pack <$> RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  products <-
    SProduct.findByName (fromMaybe "DONT MATCH" (productCase.udf1))
      >>= fromMaybeM ProductsNotFound
  return
    ProductInstance.ProductInstance
      { id = productInstanceId,
        caseId = productCase.id,
        productId = products.id,
        personId = Nothing,
        personUpdatedAt = Nothing,
        shortId = ShortId shortId,
        entityType = ProductInstance.VEHICLE,
        entityId = Nothing,
        quantity = 1,
        _type = Case.RIDESEARCH,
        price = price,
        status = status,
        startTime = productCase.startTime,
        endTime = productCase.endTime,
        validTill = productCase.validTill,
        fromLocation = Just $ productCase.fromLocationId,
        toLocation = Just $ productCase.toLocationId,
        organizationId = transporterId,
        parentId = Nothing,
        distance = 0,
        udf1 = show <$> nearestDriverDist,
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
  [ProductInstance.ProductInstance] ->
  Org.Organization ->
  m API.OnSearchServices
mkOnSearchPayload productCase productInstances transporterOrg = do
  ProductInstance.getCountByStatus (transporterOrg.id) Case.RIDEORDER
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

mkProviderStats :: [(ProductInstance.ProductInstanceStatus, Int)] -> APICase.ProviderStats
mkProviderStats piCount =
  APICase.ProviderStats
    { completed = List.lookup ProductInstance.COMPLETED piCount,
      inprogress = List.lookup ProductInstance.INPROGRESS piCount,
      confirmed = List.lookup ProductInstance.CONFIRMED piCount
    }
