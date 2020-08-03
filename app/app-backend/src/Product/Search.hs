{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import App.Types
import Beckn.Types.API.Search
import Beckn.Types.App as TA
import Beckn.Types.Common
import Beckn.Types.Core.DecimalValue (convertDecimalValueToAmount)
import qualified Beckn.Types.Core.Item as Core
import qualified Beckn.Types.Core.Provider as Core
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Service as BM
import Beckn.Types.Mobility.Stop as BS
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (encodeToText, withFlowHandler)
import Beckn.Utils.Extra
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as Case
import qualified Models.Product as Products
import qualified Models.ProductInstance as ProductInstance
import Servant
import qualified Storage.Queries.Location as Location
import System.Environment
import qualified Types.API.Common as API
import qualified Types.API.Search as API
import Types.ProductInfo
import Utils.Common (generateShortId)
import qualified Utils.Metrics as Metrics
import qualified Utils.Notifications as Notify

search :: Person.Person -> SearchReq -> FlowHandler API.AckResponse
search person req = withFlowHandler $ do
  validateDateTime req
  fromLocation <- mkLocation $ req ^. #message . #intent . #_origin
  toLocation <- mkLocation $ req ^. #message . #intent . #_destination
  Location.create fromLocation
  Location.create toLocation
  case_ <- mkCase req (_getPersonId $ person ^. #_id) fromLocation toLocation
  Case.create case_
  Metrics.incrementCaseCount Case.NEW Case.RIDESEARCH
  gatewayUrl <- Gateway.getGatewayBaseUrl
  eres <- Gateway.search gatewayUrl $ req & #context . #_request_transaction_id .~ _getCaseId (case_ ^. #_id)
  let sAck =
        case eres of
          Left err -> API.Ack "Error" (show err)
          Right _ -> API.Ack "Successful" (_getCaseId $ case_ ^. #_id)
  return $ API.AckResponse (req ^. #context) sAck Nothing
  where
    validateDateTime sreq = do
      currTime <- getCurrentTimeUTC
      when ((sreq ^. #message . #intent . #_origin . #_departure_time . #_est) < currTime) $
        L.throwException $
          err400 {errBody = "Invalid start time"}

searchCb :: () -> OnSearchReq -> FlowHandler OnSearchRes
searchCb _unit req = withFlowHandler $ do
  -- TODO: Verify api key here
  let (services :: [Service]) = req ^. #message . #services
  traverse_ (searchCbService req) services
  return $ AckResponse (req ^. #context) (ack "ACK") Nothing

searchCbService :: OnSearchReq -> BM.Service -> Flow OnSearchRes
searchCbService req service = do
  -- TODO: Verify api key here
  let mprovider = service ^. #_provider
      mcatalog = service ^. #_catalog
      caseId = CaseId $ req ^. #context . #_request_transaction_id --CaseId $ service ^. #_id
  case mcatalog of
    Nothing -> return ()
    Just catalog -> do
      case_ <- Case.findByIdAndType caseId Case.RIDESEARCH
      when
        (case_ ^. #_status == Case.CLOSED)
        (L.throwException $ err400 {errBody = "Case expired"})
      personId <-
        maybe
          (L.throwException $ err500 {errBody = "No person linked to case"})
          (return . PersonId)
          (Case._requestor case_)
      let items = catalog ^. #_items
      products <- traverse (mkProduct case_ mprovider) items
      traverse_ Products.create products
      productInstances <- traverse (mkProductInstance case_ mprovider personId) items
      traverse_ ProductInstance.create productInstances
      extendCaseExpiry case_
      Notify.notifyOnSearchCb personId case_ productInstances
  return $ AckResponse (req ^. #context) (ack "ACK") Nothing
  where
    extendCaseExpiry :: Case.Case -> Flow ()
    extendCaseExpiry Case.Case {..} = do
      now <- getCurrentTimeUTC
      confirmExpiry <-
        fromMaybe 1800 . (readMaybe =<<)
          <$> L.runIO (lookupEnv "DEFAULT_CONFIRM_EXPIRY")
      let newValidTill = fromInteger confirmExpiry `addLocalTime` now
      when (_validTill < newValidTill) $ Case.updateValidTill _id newValidTill

mkCase :: SearchReq -> Text -> Location.Location -> Location.Location -> Flow Case.Case
mkCase req userId from to = do
  now <- getCurrentTimeUTC
  cid <- generateGUID
  -- TODO: consider collision probability for shortId
  -- Currently it's a random 10 char alphanumeric string
  -- If the insert fails, maybe retry automatically as there
  -- is a unique constraint on `shortId`
  shortId <- generateShortId
  let (intent :: Intent) = req ^. #message . #intent
      context = req ^. #context
  validTill <- getCaseExpiry $ req ^. #message . #intent . #_origin . #_departure_time . #_est
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
        _startTime = req ^. #message . #intent . #_origin . #_departure_time . #_est,
        _endTime = Nothing,
        _validTill = validTill,
        _provider = Nothing,
        _providerType = Nothing,
        _requestor = Just userId,
        _requestorType = Just Case.CONSUMER,
        _parentCaseId = Nothing,
        _fromLocationId = TA._getLocationId $ from ^. #_id,
        _toLocationId = TA._getLocationId $ to ^. #_id,
        _udf1 = Just $ intent ^. #_vehicle . #variant,
        _udf2 = Just $ show $ length $ intent ^. #_payload . #_travellers,
        _udf3 = Nothing,
        _udf4 = Just $ context ^. #_request_transaction_id,
        _udf5 = Nothing,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now
      }
  where
    getCaseExpiry :: LocalTime -> Flow LocalTime
    getCaseExpiry startTime = do
      now <- getCurrentTimeUTC
      caseExpiryEnv <- L.runIO $ lookupEnv "DEFAULT_CASE_EXPIRY"
      let caseExpiry = fromMaybe 7200 $ readMaybe =<< caseExpiryEnv
          minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffLocalTime` now
          validTill = addLocalTime (minimum [fromInteger caseExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

mkLocation :: BS.Stop -> Flow Location.Location
mkLocation BS.Stop {..} = do
  let loc = _location
  now <- getCurrentTimeUTC
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

mkProduct :: Case.Case -> Maybe Core.Provider -> Core.Item -> Flow Products.Products
mkProduct case_ _mprovider item = do
  now <- getCurrentTimeUTC
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

mkProductInstance :: Case.Case -> Maybe Core.Provider -> PersonId -> Core.Item -> Flow ProductInstance.ProductInstance
mkProductInstance case_ mprovider personId item = do
  now <- getCurrentTimeUTC
  let info = ProductInfo mprovider Nothing
  price <-
    case convertDecimalValueToAmount =<< item ^. #_price . #_listed_value of
      Nothing -> L.throwException $ err400 {errBody = "Invalid price"}
      Just p -> return p
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    ProductInstance.ProductInstance
      { _id = ProductInstanceId $ item ^. #_id,
        _shortId = "",
        _caseId = case_ ^. #_id,
        _productId = ProductsId $ item ^. #_id, -- TODO needs to be fixed
        _personId = Just personId,
        _quantity = 1,
        _entityType = ProductInstance.VEHICLE,
        _status = ProductInstance.INSTOCK,
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
        _udf5 = Nothing,
        _fromLocation = Just $ case_ ^. #_fromLocationId,
        _toLocation = Just $ case_ ^. #_toLocationId,
        _info = Just $ encodeToText info,
        _organizationId = "", -- TODO: fix this
        _createdAt = now,
        _updatedAt = now
      }
