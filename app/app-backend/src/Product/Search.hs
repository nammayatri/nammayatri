{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Search as Search
import Beckn.Types.Core.Ack
import Beckn.Types.Core.DecimalValue (convertDecimalValueToAmount)
import qualified Beckn.Types.Core.Item as Core
import Beckn.Types.Core.Tag
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Types.Mobility.Catalog as BM
import Beckn.Types.Mobility.Stop as BS
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.Products as Products
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Models.Case as Case
import qualified Models.ProductInstance as MPI
import Product.Serviceability
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.Products as QProducts
import qualified Types.API.Case as API
import qualified Types.API.Search as API
import Types.API.Serviceability
import qualified Types.Common as Common
import Types.Error
import qualified Types.Metrics as Metrics
import Types.ProductInfo
import Utils.Common

search :: Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search person req = withFlowHandlerAPI $ do
  validateDateTime req
  validateServiceability req
  fromLocation <- mkLocation $ toBeckn $ req.origin
  toLocation <- mkLocation $ toBeckn $ req.destination
  case_ <- mkCase req (getId $ person.id) fromLocation toLocation
  Metrics.incrementCaseCount Case.NEW Case.RIDESEARCH
  let txnId = getId (case_.id)
  Metrics.startSearchMetrics txnId
  DB.runSqlDBTransaction $ do
    Location.create fromLocation
    Location.create toLocation
    QCase.create case_
  env <- ask
  let bapNwAddr = env.bapNwAddress
  context <- buildContext "search" txnId (Just bapNwAddr) Nothing
  let intent = mkIntent req
      tags = Just [Tag "distance" (fromMaybe "" $ case_.udf5)]
  ExternalAPI.search (xGatewayUri env) (Search.SearchReq context $ Search.SearchIntent (intent & #tags .~ tags))
  return $ API.SearchRes txnId
  where
    validateDateTime sreq = do
      currTime <- getCurrentTime
      let allowedStartTime = addUTCTime (-2 * 60) currTime
      when ((sreq.origin.departureTime.estimated) < allowedStartTime) $
        throwError $ InvalidRequest "Invalid start time."
    validateServiceability sreq = do
      originGps <-
        sreq.origin.location.gps
          & fromMaybeM (InvalidRequest "GPS coordinates required for the origin location")
      destinationGps <-
        req.destination.location.gps
          & fromMaybeM (InvalidRequest "GPS coordinates required for the destination location")
      let serviceabilityReq = RideServiceabilityReq originGps destinationGps
      unlessM (rideServiceable serviceabilityReq) $
        throwError $ ProductNotServiceable "due to georestrictions"

searchCb :: Org.Organization -> Search.OnSearchReq -> FlowHandler Search.OnSearchRes
searchCb _bppOrg req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_search" $ req.context
    Metrics.finishSearchMetrics $ req.context.transaction_id
    case req.contents of
      Right msg -> do
        let catalog = msg.catalog
        searchCbService req catalog
      Left err -> logTagError "on_search req" $ "on_search error: " <> show err
    return Ack

searchCbService :: (HasFlowEnv m r '["searchConfirmExpiry" ::: Maybe Integer], HasFlowDBEnv m r) => Search.OnSearchReq -> BM.Catalog -> m ()
searchCbService req catalog = do
  let caseId = Id $ req.context.transaction_id --CaseId $ service.id
  case_ <- Case.findByIdAndType caseId Case.RIDESEARCH
  when (case_.status /= Case.CLOSED) $ do
    bpp <-
      Org.findOrganizationByCallbackUri (req.context.bpp_uri) Org.PROVIDER
        >>= fromMaybeM OrgDoesNotExist
    personId <- (Id <$> Case.requestor case_) & fromMaybeM (CaseFieldNotPresent "requestor")
    transaction <- case (catalog.categories, catalog.items) of
      ([], _) -> throwError $ InvalidRequest "Missing provider"
      (category : _, []) -> do
        let provider = fromBeckn category
        declinedPI <- mkDeclinedProductInstance case_ bpp provider personId
        return $ QPI.create declinedPI
      (category : _, items) -> do
        when
          (case_.status == Case.CLOSED)
          (throwError CaseExpired)
        let provider = fromBeckn category
        products <- traverse (mkProduct case_) items
        productInstances <- traverse (mkProductInstance case_ bpp provider personId) items
        currTime <- getCurrentTime
        confirmExpiry <- fromMaybe 1800 <$> asks (.searchConfirmExpiry)
        let newValidTill = fromInteger confirmExpiry `addUTCTime` currTime
        return $ do
          traverse_ QProducts.create products
          traverse_ QPI.create productInstances
          when (case_.validTill < newValidTill) $ QCase.updateValidTill (case_.id) newValidTill
    piList <- MPI.findAllByCaseId (case_.id)
    let piStatusCount = Map.fromListWith (+) $ zip (PI.status <$> piList) $ repeat (1 :: Integer)
        accepted = Map.lookup PI.INSTOCK piStatusCount
        declined = Map.lookup PI.OUTOFSTOCK piStatusCount
        mCaseInfo :: (Maybe API.CaseInfo) = decodeFromText =<< (case_.info)

    DB.runSqlDBTransaction $ do
      transaction
      whenJust mCaseInfo $ \info -> do
        let uInfo = info & #accepted .~ accepted & #declined .~ declined
        QCase.updateInfo (case_.id) (encodeToText uInfo)

mkCase ::
  ( (HasFlowEnv m r ["searchCaseExpiry" ::: Maybe Integer, "graphhopperUrl" ::: BaseUrl]),
    HasFlowDBEnv m r
  ) =>
  API.SearchReq ->
  Text ->
  Location.Location ->
  Location.Location ->
  m Case.Case
mkCase req userId from to = do
  now <- getCurrentTime
  cid <- generateGUID
  distance <- getDistance req
  orgs <- Org.listOrganizations Nothing Nothing [Org.PROVIDER] [Org.APPROVED]
  let info = encodeToText $ API.CaseInfo (Just $ toInteger $ length orgs) (Just 0) (Just 0)
  -- TODO: consider collision probability for shortId
  -- Currently it's a random 10 char alphanumeric string
  -- If the insert fails, maybe retry automatically as there
  -- is a unique constraint on `shortId`
  shortId_ <- generateShortId
  validTill <- getCaseExpiry $ req.origin.departureTime.estimated
  return
    Case.Case
      { id = cid,
        name = Nothing,
        description = Just "Case to search for a Ride",
        shortId = shortId_,
        industry = Case.MOBILITY,
        _type = Case.RIDESEARCH,
        exchangeType = Case.FULFILLMENT,
        status = Case.NEW,
        startTime = req.origin.departureTime.estimated,
        endTime = Nothing,
        validTill = validTill,
        provider = Nothing,
        providerType = Nothing,
        requestor = Just userId,
        requestorType = Just Case.CONSUMER,
        parentCaseId = Nothing,
        fromLocationId = from.id,
        toLocationId = to.id,
        udf1 = Just $ req.vehicle.variant,
        udf2 = Just . show . length $ req.travellers,
        udf3 = Nothing,
        udf4 = Just $ req.transaction_id,
        udf5 = show <$> distance,
        info = Just info,
        createdAt = now,
        updatedAt = now
      }
  where
    getCaseExpiry :: (HasFlowEnv m r '["searchCaseExpiry" ::: Maybe Integer]) => UTCTime -> m UTCTime
    getCaseExpiry startTime = do
      now <- getCurrentTime
      caseExpiry <- fromMaybe 7200 <$> asks (.searchCaseExpiry)
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger caseExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

mkLocation :: MonadFlow m => BS.Stop -> m Location.Location
mkLocation BS.Stop {..} = do
  let loc = location
  now <- getCurrentTime
  locId <- generateGUID
  let mgps = loc.gps
  return
    Location.Location
      { id = locId,
        locationType = Location.POINT,
        lat = read . T.unpack . (.lat) <$> mgps,
        long = read . T.unpack . (.lon) <$> mgps,
        ward = Nothing,
        district = Nothing,
        city = (.name) <$> loc.city,
        state = (.state) <$> loc.address,
        country = (.name) <$> loc.country,
        pincode = Nothing,
        address = T.decodeUtf8 . BSL.toStrict . encode <$> loc.address,
        bound = Nothing,
        point = Location.Point,
        createdAt = now,
        updatedAt = now
      }

mkProduct :: MonadFlow m => Case.Case -> Core.Item -> m Products.Products
mkProduct case_ item = do
  now <- getCurrentTime
  price <-
    case convertDecimalValueToAmount =<< item.price.listed_value of
      Nothing -> throwError $ InvalidRequest "convertDecimalValueToAmount returns Nothing."
      Just p -> return p
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    Products.Products
      { id = Id $ item.id,
        shortId = "",
        name = fromMaybe "" $ item.descriptor.name,
        description = item.descriptor.short_desc,
        industry = case_.industry,
        _type = Products.RIDE,
        status = Products.INSTOCK,
        price = price,
        rating = Nothing,
        review = Nothing,
        udf1 = Nothing,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        info = Nothing,
        createdAt = now,
        updatedAt = now
      }

mkProductInstance ::
  MonadFlow m =>
  Case.Case ->
  Org.Organization ->
  Common.Provider ->
  Id Person.Person ->
  Core.Item ->
  m PI.ProductInstance
mkProductInstance case_ bppOrg provider personId item = do
  now <- getCurrentTime
  let info = ProductInfo (Just provider) Nothing
      price = convertDecimalValueToAmount =<< item.price.listed_value
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    PI.ProductInstance
      { id = Id $ item.id,
        shortId = "",
        caseId = case_.id,
        productId = Id $ item.id, -- TODO needs to be fixed
        personId = Just personId,
        personUpdatedAt = Nothing,
        quantity = 1,
        entityType = PI.VEHICLE,
        status = PI.INSTOCK,
        startTime = case_.startTime,
        endTime = case_.endTime,
        validTill = case_.validTill,
        parentId = Nothing,
        entityId = Nothing,
        price = price,
        _type = Case.RIDESEARCH,
        udf1 = Nothing,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = case_.udf5,
        fromLocation = Just $ case_.fromLocationId,
        toLocation = Just $ case_.toLocationId,
        info = Just $ encodeToText info,
        organizationId = bppOrg.id,
        createdAt = now,
        updatedAt = now
      }

mkDeclinedProductInstance :: MonadFlow m => Case.Case -> Org.Organization -> Common.Provider -> Id Person.Person -> m PI.ProductInstance
mkDeclinedProductInstance case_ bppOrg provider personId = do
  now <- getCurrentTime
  piId <- generateGUID
  let info = ProductInfo (Just provider) Nothing
  return
    PI.ProductInstance
      { id = Id piId,
        shortId = "",
        caseId = case_.id,
        productId = Id piId,
        personId = Just personId,
        personUpdatedAt = Nothing,
        quantity = 1,
        entityType = PI.VEHICLE,
        status = PI.OUTOFSTOCK,
        startTime = case_.startTime,
        endTime = case_.endTime,
        validTill = case_.validTill,
        parentId = Nothing,
        entityId = Nothing,
        price = Nothing,
        _type = Case.RIDESEARCH,
        udf1 = Nothing,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        fromLocation = Just $ case_.fromLocationId,
        toLocation = Just $ case_.toLocationId,
        info = Just $ encodeToText info,
        organizationId = bppOrg.id,
        createdAt = now,
        updatedAt = now
      }

getDistance :: HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl] => API.SearchReq -> m (Maybe Float)
getDistance req =
  mkRouteRequest (req.origin.location) (req.destination.location)
    >>= MapSearch.getRouteMb
    <&> fmap MapSearch.distanceInM

mkRouteRequest :: MonadFlow m => Common.Location -> Common.Location -> m MapSearch.Request
mkRouteRequest pickupLoc dropLoc = do
  (Common.GPS pickupLat pickupLon) <- Common.gps pickupLoc & fromMaybeM (InvalidRequest "No long / lat.")
  (Common.GPS dropLat dropLon) <- Common.gps dropLoc & fromMaybeM (InvalidRequest "No long / lat.")
  pickupMapPoint <- mkMapPoint pickupLat pickupLon
  dropMapPoint <- mkMapPoint dropLat dropLon
  return $
    MapSearch.Request
      { waypoints = [pickupMapPoint, dropMapPoint],
        mode = Just MapSearch.CAR,
        departureTime = Nothing,
        arrivalTime = Nothing,
        calcPoints = Just False
      }

mkMapPoint :: MonadFlow m => Text -> Text -> m MapSearch.MapPoint
mkMapPoint lat' lon' = do
  lat <- readLatLng lat'
  lon <- readLatLng lon'
  return $ MapSearch.LatLong $ MapSearch.PointXY lat lon

readLatLng :: MonadFlow m => Text -> m Double
readLatLng text = do
  readMaybe (T.unpack text) & fromMaybeM (InternalError "Location read error")
