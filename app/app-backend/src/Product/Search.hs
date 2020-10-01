{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App as TA
import Beckn.Types.Common
import Beckn.Types.Core.DecimalValue (convertDecimalValueToAmount)
import qualified Beckn.Types.Core.Item as Core
import Beckn.Types.Core.Tag
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Types.Mobility.Catalog as BM
import Beckn.Types.Mobility.Stop as BS
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as Case
import qualified Models.Product as Products
import qualified Models.ProductInstance as MPI
import Servant
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Organization as Org
import qualified Types.API.Case as API
import qualified Types.API.Common as API
import qualified Types.API.Search as API
import qualified Types.Common as Common
import Types.ProductInfo
import Utils.Common (generateShortId, mkContext, mkIntent, validateContext)
import qualified Utils.Metrics as Metrics
import qualified Utils.Notifications as Notify

search :: Person.Person -> API.SearchReq -> FlowHandler API.AckResponse
search person req = withFlowHandler $ do
  validateDateTime req
  fromLocation <- mkLocation $ toBeckn $ req ^. #origin
  toLocation <- mkLocation $ toBeckn $ req ^. #destination
  Location.create fromLocation
  Location.create toLocation
  case_ <- mkCase req (_getPersonId $ person ^. #_id) fromLocation toLocation
  Case.create case_
  Metrics.incrementCaseCount Case.NEW Case.RIDESEARCH
  now <- L.runIO getCurrentTime
  env <- ask
  let context = mkContext "search" (_getCaseId (case_ ^. #_id)) now (bapNwAddress env) Nothing
      intent = mkIntent req
      tags = Just [Tag "distance" (fromMaybe "" $ case_ ^. #_udf5)]
  eres <- Gateway.search (xGatewayUri env) $ Search.SearchReq context $ Search.SearchIntent (intent & #_tags .~ tags)
  let sAck =
        case eres of
          Left err -> API.Ack "Error" (show err)
          Right _ -> API.Ack "Successful" (_getCaseId $ case_ ^. #_id)
  return $ API.AckResponse (_getCaseId (case_ ^. #_id)) sAck Nothing
  where
    validateDateTime sreq = do
      currTime <- getCurrTime
      let allowedStartTime = addUTCTime (-2 * 60) currTime
      when ((sreq ^. #origin . #departureTime . #estimated) < allowedStartTime) $
        L.throwException $
          err400 {errBody = "Invalid start time"}

searchCb :: () -> Search.OnSearchReq -> FlowHandler Search.OnSearchRes
searchCb _unit req = withFlowHandler $ do
  validateContext "on_search" $ req ^. #context
  case req ^. #contents of
    Right msg -> do
      let catalog = msg ^. #catalog
      _ <- searchCbService req catalog
      return ()
    Left err -> L.logError @Text "on_search req" $ "on_search error: " <> show err
  return $ AckResponse (req ^. #context) (ack "ACK") Nothing

searchCbService :: Search.OnSearchReq -> BM.Catalog -> Flow Search.OnSearchRes
searchCbService req catalog = do
  let caseId = CaseId $ req ^. #context . #_transaction_id --CaseId $ service ^. #_id
  case_ <- Case.findByIdAndType caseId Case.RIDESEARCH
  when (case_ ^. #_status /= Case.CLOSED) $ do
    bpp <-
      Org.findOrganizationByCallbackUri (req ^. #context . #_bpp_uri) Org.PROVIDER
        >>= fromMaybeM400 "INVALID_PROVIDER_URI"
    personId <-
      maybe
        (L.throwException $ err500 {errBody = "No person linked to case"})
        (return . PersonId)
        (Case._requestor case_)
    case (catalog ^. #_categories, catalog ^. #_items) of
      ([], _) -> L.throwException $ err400 {errBody = "missing provider"}
      (category : _, []) -> do
        let provider = fromBeckn category
        declinedPI <- mkDeclinedProductInstance case_ bpp provider personId
        MPI.create declinedPI
        return ()
      (category : _, items) -> do
        when
          (case_ ^. #_status == Case.CLOSED)
          (L.throwException $ err400 {errBody = "Case expired"})
        let provider = fromBeckn category
        products <- traverse (mkProduct case_) items
        traverse_ Products.create products
        productInstances <- traverse (mkProductInstance case_ bpp provider personId) items
        traverse_ MPI.create productInstances
        extendCaseExpiry case_
        Notify.notifyOnSearchCb personId case_ productInstances
    piList <- MPI.findAllByCaseId (case_ ^. #_id)
    let piStatusCount = Map.fromListWith (+) $ zip (PI._status <$> piList) $ repeat (1 :: Integer)
        accepted = Map.lookup PI.INSTOCK piStatusCount
        declined = Map.lookup PI.OUTOFSTOCK piStatusCount
        mCaseInfo :: (Maybe API.CaseInfo) = decodeFromText =<< (case_ ^. #_info)
    whenJust mCaseInfo $ \info -> do
      let uInfo = info & #_accepted .~ accepted & #_declined .~ declined
      Case.updateInfo (case_ ^. #_id) (encodeToText uInfo)
  return $ AckResponse (req ^. #context) (ack "ACK") Nothing
  where
    extendCaseExpiry :: Case.Case -> Flow ()
    extendCaseExpiry Case.Case {..} = do
      now <- getCurrTime
      confirmExpiry <- fromMaybe 1800 . searchConfirmExpiry <$> ask
      let newValidTill = fromInteger confirmExpiry `addUTCTime` now
      when (_validTill < newValidTill) $ Case.updateValidTill _id newValidTill

mkCase :: API.SearchReq -> Text -> Location.Location -> Location.Location -> Flow Case.Case
mkCase req userId from to = do
  now <- getCurrTime
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
        _fromLocationId = TA._getLocationId $ from ^. #_id,
        _toLocationId = TA._getLocationId $ to ^. #_id,
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
      now <- getCurrTime
      caseExpiry <- fromMaybe 7200 . searchCaseExpiry <$> ask
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger caseExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

mkLocation :: BS.Stop -> Flow Location.Location
mkLocation BS.Stop {..} = do
  let loc = _location
  now <- getCurrTime
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
        _state = Nothing,
        _country = (^. #name) <$> loc ^. #_country,
        _pincode = Nothing,
        _address = T.decodeUtf8 . BSL.toStrict . encode <$> loc ^. #_address,
        _bound = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

mkProduct :: Case.Case -> Core.Item -> Flow Products.Products
mkProduct case_ item = do
  now <- getCurrTime
  price <-
    case convertDecimalValueToAmount =<< item ^. #_price . #_listed_value of
      Nothing -> L.throwException $ err400 {errBody = "Invalid price"}
      Just p -> return p
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    Products.Products
      { _id = ProductsId $ item ^. #_id,
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
  Case.Case -> Org.Organization -> Common.Provider -> PersonId -> Core.Item -> Flow PI.ProductInstance
mkProductInstance case_ bppOrg provider personId item = do
  now <- getCurrTime
  let info = ProductInfo (Just provider) Nothing
  price <-
    case convertDecimalValueToAmount =<< item ^. #_price . #_listed_value of
      Nothing -> L.throwException $ err400 {errBody = "Invalid price"}
      Just p -> return p
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    PI.ProductInstance
      { _id = ProductInstanceId $ item ^. #_id,
        _shortId = "",
        _caseId = case_ ^. #_id,
        _productId = ProductsId $ item ^. #_id, -- TODO needs to be fixed
        _personId = Just personId,
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
        _organizationId = _getOrganizationId $ bppOrg ^. #_id,
        _createdAt = now,
        _updatedAt = now
      }

mkDeclinedProductInstance :: Case.Case -> Org.Organization -> Common.Provider -> PersonId -> Flow PI.ProductInstance
mkDeclinedProductInstance case_ bppOrg provider personId = do
  now <- getCurrTime
  piId <- generateGUID
  let info = ProductInfo (Just provider) Nothing
  return
    PI.ProductInstance
      { _id = ProductInstanceId piId,
        _shortId = "",
        _caseId = case_ ^. #_id,
        _productId = ProductsId piId,
        _personId = Just personId,
        _quantity = 1,
        _entityType = PI.VEHICLE,
        _status = PI.OUTOFSTOCK,
        _startTime = case_ ^. #_startTime,
        _endTime = case_ ^. #_endTime,
        _validTill = case_ ^. #_validTill,
        _parentId = Nothing,
        _entityId = Nothing,
        _price = 0,
        _type = Case.RIDESEARCH,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _udf3 = Nothing,
        _udf4 = Nothing,
        _udf5 = Nothing,
        _fromLocation = Just $ case_ ^. #_fromLocationId,
        _toLocation = Just $ case_ ^. #_toLocationId,
        _info = Just $ encodeToText info,
        _organizationId = _getOrganizationId $ bppOrg ^. #_id,
        _createdAt = now,
        _updatedAt = now
      }

getDistance :: API.SearchReq -> Flow (Maybe Float)
getDistance req = do
  routeReq <- mkRouteRequest (req ^. #origin . #location) (req ^. #destination . #location)
  distRes <- MapSearch.getRoute routeReq
  case distRes of
    Left _ -> return Nothing
    Right MapSearch.Response {..} ->
      return $ MapSearch.distanceInM <$> headMaybe routes

mkRouteRequest :: Common.Location -> Common.Location -> Flow MapSearch.Request
mkRouteRequest pickupLoc dropLoc = do
  (Common.GPS pickupLat pickupLon) <- Common.gps pickupLoc & fromMaybeM400 "LAT_LON_NOT_FOUND"
  (Common.GPS dropLat dropLon) <- Common.gps dropLoc & fromMaybeM400 "LAT_LON_NOT_FOUND"
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
  let mCoord = readMaybe $ T.unpack text
  maybe (throwJsonError400 "ERR" "LOCATION_READ_ERROR") pure mCoord
