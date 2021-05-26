{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
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
import EulerHS.Prelude
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
import Types.ProductInfo
import Utils.Common
import qualified Utils.Metrics as Metrics

search :: Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search person req = withFlowHandlerAPI $ do
  validateDateTime req
  validateServiceability req
  fromLocation <- mkLocation $ toBeckn $ req ^. #origin
  toLocation <- mkLocation $ toBeckn $ req ^. #destination
  case_ <- mkCase req (getId $ person ^. #_id) fromLocation toLocation
  Metrics.incrementCaseCount Case.NEW Case.RIDESEARCH
  let txnId = getId (case_ ^. #_id)
  Metrics.startSearchMetrics txnId
  DB.runSqlDBTransaction $ do
    Location.create fromLocation
    Location.create toLocation
    QCase.create case_
  env <- ask
  let bapNwAddr = env ^. #bapNwAddress
  context <- buildContext "search" txnId (Just bapNwAddr) Nothing
  let intent = mkIntent req
      tags = Just [Tag "distance" (fromMaybe "" $ case_ ^. #_udf5)]
  ExternalAPI.search (xGatewayUri env) (Search.SearchReq context $ Search.SearchIntent (intent & #_tags .~ tags))
    >>= checkAckResponseError (ExternalAPIResponseError "search")
  return $ API.SearchRes txnId
  where
    validateDateTime sreq = do
      currTime <- getCurrentTime
      let allowedStartTime = addUTCTime (-2 * 60) currTime
      when ((sreq ^. #origin . #departureTime . #estimated) < allowedStartTime) $
        throwError $ InvalidRequest "Invalid start time."
    validateServiceability sreq = do
      originGps <-
        sreq ^. #origin . #location . #gps
          & fromMaybeM (InvalidRequest "GPS coordinates required for the origin location")
      destinationGps <-
        req ^. #destination . #location . #gps
          & fromMaybeM (InvalidRequest "GPS coordinates required for the destination location")
      let serviceabilityReq = RideServiceabilityReq originGps destinationGps
      unlessM (rideServiceable serviceabilityReq) $
        throwError $ ProductNotServiceable "due to georestrictions"

searchCb :: Org.Organization -> Search.OnSearchReq -> FlowHandler Search.OnSearchRes
searchCb _bppOrg req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_search" $ req ^. #context
    Metrics.finishSearchMetrics $ req ^. #context . #_transaction_id
    case req ^. #contents of
      Right msg -> do
        let catalog = msg ^. #catalog
        searchCbService req catalog
      Left err -> logTagError "on_search req" $ "on_search error: " <> show err
    return Ack

searchCbService :: Search.OnSearchReq -> BM.Catalog -> Flow ()
searchCbService req catalog = do
  let caseId = Id $ req ^. #context . #_transaction_id --CaseId $ service ^. #_id
  case_ <- Case.findByIdAndType caseId Case.RIDESEARCH
  when (case_ ^. #_status /= Case.CLOSED) $ do
    bpp <-
      Org.findOrganizationByCallbackUri (req ^. #context . #_bpp_uri) Org.PROVIDER
        >>= fromMaybeM OrgDoesNotExist
    personId <- (Id <$> Case._requestor case_) & fromMaybeM (CaseFieldNotPresent "requestor")
    transaction <- case (catalog ^. #_categories, catalog ^. #_items) of
      ([], _) -> throwError $ InvalidRequest "Missing provider"
      (category : _, []) -> do
        let provider = fromBeckn category
        declinedPI <- mkDeclinedProductInstance case_ bpp provider personId
        return $ QPI.create declinedPI
      (category : _, items) -> do
        when
          (case_ ^. #_status == Case.CLOSED)
          (throwError CaseExpired)
        let provider = fromBeckn category
        products <- traverse (mkProduct case_) items
        productInstances <- traverse (mkProductInstance case_ bpp provider personId) items
        currTime <- getCurrentTime
        confirmExpiry <- fromMaybe 1800 . searchConfirmExpiry <$> ask
        let newValidTill = fromInteger confirmExpiry `addUTCTime` currTime
        return $ do
          traverse_ QProducts.create products
          traverse_ QPI.create productInstances
          when (case_ ^. #_validTill < newValidTill) $ QCase.updateValidTill (case_ ^. #_id) newValidTill
    piList <- MPI.findAllByCaseId (case_ ^. #_id)
    let piStatusCount = Map.fromListWith (+) $ zip (PI._status <$> piList) $ repeat (1 :: Integer)
        accepted = Map.lookup PI.INSTOCK piStatusCount
        declined = Map.lookup PI.OUTOFSTOCK piStatusCount
        mCaseInfo :: (Maybe API.CaseInfo) = decodeFromText =<< (case_ ^. #_info)

    DB.runSqlDBTransaction $ do
      transaction
      whenJust mCaseInfo $ \info -> do
        let uInfo = info & #_accepted .~ accepted & #_declined .~ declined
        QCase.updateInfo (case_ ^. #_id) (encodeToText uInfo)

mkCase :: API.SearchReq -> Text -> Location.Location -> Location.Location -> Flow Case.Case
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
  shortId <- generateShortId
  validTill <- getCaseExpiry $ req ^. #origin . #departureTime . #estimated
  return
    Case.Case
      { _id = cid,
        _name = Nothing,
        _description = Just "Case to search for a Ride",
        _shortId = shortId,
        _industry = Case.MOBILITY,
        _type = Case.RIDESEARCH,
        _exchangeType = Case.FULFILLMENT,
        _status = Case.NEW,
        _startTime = req ^. #origin . #departureTime . #estimated,
        _endTime = Nothing,
        _validTill = validTill,
        _provider = Nothing,
        _providerType = Nothing,
        _requestor = Just userId,
        _requestorType = Just Case.CONSUMER,
        _parentCaseId = Nothing,
        _fromLocationId = getId $ from ^. #_id,
        _toLocationId = getId $ to ^. #_id,
        _udf1 = Just $ req ^. #vehicle . #variant,
        _udf2 = Just $ show $ length $ req ^. #travellers,
        _udf3 = Nothing,
        _udf4 = Just $ req ^. #transaction_id,
        _udf5 = show <$> distance,
        _info = Just info,
        _createdAt = now,
        _updatedAt = now
      }
  where
    getCaseExpiry :: UTCTime -> Flow UTCTime
    getCaseExpiry startTime = do
      now <- getCurrentTime
      caseExpiry <- fromMaybe 7200 . searchCaseExpiry <$> ask
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger caseExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

mkLocation :: BS.Stop -> Flow Location.Location
mkLocation BS.Stop {..} = do
  let loc = _location
  now <- getCurrentTime
  locId <- generateGUID
  let mgps = loc ^. #_gps
  return
    Location.Location
      { _id = locId,
        _locationType = Location.POINT,
        _lat = read . T.unpack . (^. #lat) <$> mgps,
        _long = read . T.unpack . (^. #lon) <$> mgps,
        _ward = Nothing,
        _district = Nothing,
        _city = (^. #name) <$> loc ^. #_city,
        _state = (^. #_state) <$> loc ^. #_address,
        _country = (^. #name) <$> loc ^. #_country,
        _pincode = Nothing,
        _address = T.decodeUtf8 . BSL.toStrict . encode <$> loc ^. #_address,
        _bound = Nothing,
        _point = Location.Point,
        _createdAt = now,
        _updatedAt = now
      }

mkProduct :: Case.Case -> Core.Item -> Flow Products.Products
mkProduct case_ item = do
  now <- getCurrentTime
  price <-
    case convertDecimalValueToAmount =<< item ^. #_price . #_listed_value of
      Nothing -> throwError $ InvalidRequest "convertDecimalValueToAmount returns Nothing."
      Just p -> return p
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    Products.Products
      { _id = Id $ item ^. #_id,
        _shortId = "",
        _name = fromMaybe "" $ item ^. #_descriptor . #_name,
        _description = item ^. #_descriptor . #_short_desc,
        _industry = case_ ^. #_industry,
        _type = Products.RIDE,
        _status = Products.INSTOCK,
        _price = price,
        _rating = Nothing,
        _review = Nothing,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _udf3 = Nothing,
        _udf4 = Nothing,
        _udf5 = Nothing,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

mkProductInstance ::
  Case.Case -> Org.Organization -> Common.Provider -> Id Person.Person -> Core.Item -> Flow PI.ProductInstance
mkProductInstance case_ bppOrg provider personId item = do
  now <- getCurrentTime
  let info = ProductInfo (Just provider) Nothing
      price = convertDecimalValueToAmount =<< item ^. #_price . #_listed_value
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    PI.ProductInstance
      { _id = Id $ item ^. #_id,
        _shortId = "",
        _caseId = case_ ^. #_id,
        _productId = Id $ item ^. #_id, -- TODO needs to be fixed
        _personId = Just personId,
        _personUpdatedAt = Nothing,
        _quantity = 1,
        _entityType = PI.VEHICLE,
        _status = PI.INSTOCK,
        _startTime = case_ ^. #_startTime,
        _endTime = case_ ^. #_endTime,
        _validTill = case_ ^. #_validTill,
        _parentId = Nothing,
        _entityId = Nothing,
        _price = price,
        _type = Case.RIDESEARCH,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _udf3 = Nothing,
        _udf4 = Nothing,
        _udf5 = case_ ^. #_udf5,
        _fromLocation = Just $ case_ ^. #_fromLocationId,
        _toLocation = Just $ case_ ^. #_toLocationId,
        _info = Just $ encodeToText info,
        _organizationId = getId $ bppOrg ^. #_id,
        _createdAt = now,
        _updatedAt = now
      }

mkDeclinedProductInstance :: Case.Case -> Org.Organization -> Common.Provider -> Id Person.Person -> Flow PI.ProductInstance
mkDeclinedProductInstance case_ bppOrg provider personId = do
  now <- getCurrentTime
  piId <- generateGUID
  let info = ProductInfo (Just provider) Nothing
  return
    PI.ProductInstance
      { _id = Id piId,
        _shortId = "",
        _caseId = case_ ^. #_id,
        _productId = Id piId,
        _personId = Just personId,
        _personUpdatedAt = Nothing,
        _quantity = 1,
        _entityType = PI.VEHICLE,
        _status = PI.OUTOFSTOCK,
        _startTime = case_ ^. #_startTime,
        _endTime = case_ ^. #_endTime,
        _validTill = case_ ^. #_validTill,
        _parentId = Nothing,
        _entityId = Nothing,
        _price = Nothing,
        _type = Case.RIDESEARCH,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _udf3 = Nothing,
        _udf4 = Nothing,
        _udf5 = Nothing,
        _fromLocation = Just $ case_ ^. #_fromLocationId,
        _toLocation = Just $ case_ ^. #_toLocationId,
        _info = Just $ encodeToText info,
        _organizationId = getId $ bppOrg ^. #_id,
        _createdAt = now,
        _updatedAt = now
      }

getDistance :: API.SearchReq -> Flow (Maybe Float)
getDistance req =
  mkRouteRequest (req ^. #origin . #location) (req ^. #destination . #location)
    >>= MapSearch.getRouteMb
    <&> fmap MapSearch.distanceInM

mkRouteRequest :: Common.Location -> Common.Location -> Flow MapSearch.Request
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

mkMapPoint :: Text -> Text -> Flow MapSearch.MapPoint
mkMapPoint lat' lon' = do
  lat <- readLatLng lat'
  lon <- readLatLng lon'
  return $ MapSearch.LatLong $ MapSearch.PointXY lat lon

readLatLng :: Text -> Flow Double
readLatLng text = do
  readMaybe (T.unpack text) & fromMaybeM (InternalError "Location read error")
