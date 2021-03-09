{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.BecknProvider.Search (search) where

import App.Types
import Beckn.Types.Amount
import Beckn.Types.App
import qualified Beckn.Types.Core.API.Callback as Callback
import qualified Beckn.Types.Core.API.Search as API
import qualified Beckn.Types.Core.Ack as Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Domain as Domain
import qualified Beckn.Types.Core.Error as Core
import qualified Beckn.Types.Core.Tag as Tag
import qualified Beckn.Types.Mobility.Stop as Stop
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Common
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified External.Gateway.Transform as GT
import qualified Models.Case as Case
import qualified Models.ProductInstance as MPI
import qualified Product.BecknProvider.BP as BP
import Product.FareCalculator
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Storage.Queries.Location as Loc
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as SProduct
import qualified Test.RandomStrings as RS
import qualified Types.API.Case as APICase

search :: OrganizationId -> Org.Organization -> API.SearchReq -> FlowHandler Ack.AckResponse
search transporterId bapOrg req = withFlowHandler $ do
  BP.validateContext "search" $ req ^. #context
  uuid <- L.generateGUID
  transporter <- Org.findOrganizationById transporterId
  when (transporter ^. #_enabled) $ do
    let intent = req ^. #message . #intent
    now <- getCurrTime
    let pickup = head $ intent ^. #_pickups
    let dropOff = head $ intent ^. #_drops
    let startTime = pickup ^. #_departure_time . #_est
    validity <- getValidTime now startTime
    fromLocation <- mkFromStop now pickup
    toLocation <- mkFromStop now dropOff
    Loc.create fromLocation
    Loc.create toLocation
    let bapOrgId = bapOrg ^. #_id
    deadDistance <- calculateDeadDistance transporter fromLocation
    let productCase = mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance
    Case.create productCase
    fork "OnSearchCallback" $ onSearchCallback productCase transporter fromLocation toLocation
  mkAckResponse uuid "search"

mkFromStop :: UTCTime -> Stop.Stop -> Flow Location.Location
mkFromStop now stop = do
  let loc = stop ^. #_location
  let mgps = loc ^. #_gps
  let maddress = loc ^. #_address
  uuid <- LocationId <$> L.generateGUID
  pure $
    Location.Location
      { _id = uuid,
        _locationType = Location.POINT,
        _lat = read . T.unpack . (^. #lat) <$> mgps,
        _long = read . T.unpack . (^. #lon) <$> mgps,
        _ward = (^. #_ward) =<< maddress,
        _district = Nothing,
        _city = (^. #_city) <$> maddress,
        _state = (^. #_state) <$> maddress,
        _country = (^. #_country) <$> maddress,
        _pincode = (^. #_area_code) <$> maddress,
        _address = encodeToText <$> maddress,
        _bound = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

getValidTime :: UTCTime -> UTCTime -> Flow UTCTime
getValidTime now startTime = do
  caseExpiry_ <- fromMaybe 7200 . caseExpiry <$> ask
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

mkCase :: API.SearchReq -> Text -> UTCTime -> UTCTime -> UTCTime -> Location.Location -> Location.Location -> OrganizationId -> OrganizationId -> Maybe Float -> Case.Case
mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance = do
  let intent = req ^. #message . #intent
  let distance = Tag._value <$> find (\x -> x ^. #_key == "distance") (fromMaybe [] $ intent ^. #_tags)
  let tId = _getOrganizationId transporterId
  let bapId = _getOrganizationId bapOrgId
  Case.Case
    { _id = CaseId {_getCaseId = uuid},
      _name = Nothing,
      _description = Just "Case to search for a Ride",
      _shortId = tId <> "_" <> req ^. #context . #_transaction_id,
      _industry = Case.MOBILITY,
      _type = Case.RIDESEARCH,
      _exchangeType = Case.FULFILLMENT,
      _status = Case.NEW,
      _startTime = startTime,
      _endTime = Nothing,
      _validTill = validity,
      _provider = Just tId,
      _providerType = Nothing,
      _requestor = Nothing,
      _requestorType = Just Case.CONSUMER,
      _parentCaseId = Nothing,
      _fromLocationId = _getLocationId $ fromLocation ^. #_id,
      _toLocationId = _getLocationId $ toLocation ^. #_id,
      _udf1 = Just $ intent ^. #_vehicle . #variant,
      _udf2 = Just $ show $ length $ intent ^. #_payload . #_travellers,
      _udf3 = encodeToText <$> deadDistance,
      _udf4 = Just bapId,
      _udf5 = distance,
      _info = Nothing, --Just $ show $ req ^. #message
      _createdAt = now,
      _updatedAt = now
    }

calculateDeadDistance :: Org.Organization -> Location.Location -> Flow (Maybe Float)
calculateDeadDistance organization fromLocation = do
  eres <- runSafeFlow do
    orgLocId <- LocationId <$> organization ^. #_locationId & fromMaybeM500 "ORG_HAS_NO_LOCATION"
    mbOrgLocation <- Loc.findLocationById orgLocId
    case mbOrgLocation of
      Nothing -> throwError500 "ORG_HAS_NO_LOCATION"
      Just orgLocation -> Location.calculateDistance orgLocation fromLocation
  case eres of
    Left err -> do
      logWarning "calculateDeadDistance" $ "Failed to calculate distance. Reason: " +|| err ||+ ""
      pure Nothing
    Right mDistance -> return mDistance

onSearchCallback :: Case.Case -> Org.Organization -> Location.Location -> Location.Location -> Flow ()
onSearchCallback productCase transporter fromLocation toLocation = do
  let transporterId = transporter ^. #_id
  result <- runSafeFlow $ do
    vehicleVariant :: Vehicle.Variant <- (productCase ^. #_udf1 >>= readMaybe . T.unpack) & fromMaybeM500 "NO_VEHICLE_VARIANT"
    pool <-
      Person.calculateDriverPool (fromLocation ^. #_id) transporterId vehicleVariant
    L.logInfo @Text "OnSearchCallback" $
      "Calculated Driver Pool for organization " +|| _getOrganizationId transporterId ||+ " with drivers " +| T.intercalate ", " (_getPersonId <$> pool) |+ ""
    let piStatus =
          if null pool
            then ProductInstance.OUTOFSTOCK
            else ProductInstance.INSTOCK
    price <- calculateFare transporterId vehicleVariant fromLocation toLocation (productCase ^. #_startTime) (productCase ^. #_udf5)
    prodInst :: ProductInstance.ProductInstance <- createProductInstance productCase price piStatus transporterId
    let caseStatus ProductInstance.INSTOCK = Case.CONFIRMED
        caseStatus _ = Case.CLOSED
    Case.updateStatus (productCase ^. #_id) (caseStatus $ prodInst ^. #_status)
    pure prodInst
  case result of
    Right prodInst -> do
      let productStatus = prodInst ^. #_status
      L.logInfo @Text "OnSearchCallback" $
        "Sending on_search callback with status " +|| productStatus ||+ " for product " +|| prodInst ^. #_id ||+ ""
      void $ sendOnSearchSuccess productCase transporter prodInst
    Left err -> do
      L.logError @Text "OnSearchCallback" $ "Error happened when sending on_search request. Error: " +|| err ||+ ""
      void $ sendOnSearchFailed productCase transporter err

createProductInstance :: Case.Case -> Maybe Amount -> ProductInstance.ProductInstanceStatus -> OrganizationId -> Flow ProductInstance.ProductInstance
createProductInstance productCase price status transporterId = do
  productInstanceId <- ProductInstanceId <$> L.generateGUID
  now <- getCurrTime
  shortId <- L.runIO $ T.pack <$> RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  products <- SProduct.findByName $ fromMaybe "DONT MATCH" (productCase ^. #_udf1)
  let productInstance =
        ProductInstance.ProductInstance
          { _id = productInstanceId,
            _caseId = productCase ^. #_id,
            _productId = products ^. #_id,
            _personId = Nothing,
            _personUpdatedAt = Nothing,
            _shortId = shortId,
            _entityType = ProductInstance.VEHICLE,
            _entityId = Nothing,
            _quantity = 1,
            _type = Case.RIDESEARCH,
            _price = fromMaybe 0 price,
            _status = status,
            _startTime = productCase ^. #_startTime,
            _endTime = productCase ^. #_endTime,
            _validTill = productCase ^. #_validTill,
            _fromLocation = Just $ productCase ^. #_fromLocationId,
            _toLocation = Just $ productCase ^. #_toLocationId,
            _organizationId = _getOrganizationId transporterId,
            _parentId = Nothing,
            _udf1 = productCase ^. #_udf1,
            _udf2 = productCase ^. #_udf2,
            _udf3 = productCase ^. #_udf3,
            _udf4 = productCase ^. #_udf4,
            _udf5 = productCase ^. #_udf5,
            _info = productCase ^. #_info,
            _createdAt = now,
            _updatedAt = now
          }
  ProductInstance.create productInstance
  pure productInstance

sendOnSearchFailed :: Case.Case -> Org.Organization -> Text -> Flow Ack.AckResponse
sendOnSearchFailed productCase transporterOrg err = do
  currTime <- getCurrTime
  appEnv <- ask
  let context =
        Context.Context
          { _domain = Domain.MOBILITY,
            _country = Just "IND",
            _city = Nothing,
            _action = "on_search",
            _core_version = Just "0.8.2",
            _domain_version = Just "0.8.2",
            _transaction_id = last $ T.split (== '_') $ productCase ^. #_shortId,
            _message_id = productCase ^. #_shortId,
            _bap_uri = Nothing,
            _bpp_uri = Just $ BP.makeBppUrl transporterOrg $ nwAddress appEnv,
            _timestamp = currTime,
            _ttl = Nothing
          }
  let payload =
        Callback.CallbackReq
          { context,
            contents =
              Left $
                Core.Error
                  { _type = "DOMAIN-ERROR",
                    _code = err,
                    _path = Nothing,
                    _message = Nothing
                  }
          }
  let bppShortId = _getShortOrganizationId $ transporterOrg ^. #_shortId
  Gateway.onSearch payload bppShortId

sendOnSearchSuccess :: Case.Case -> Org.Organization -> ProductInstance.ProductInstance -> Flow Ack.AckResponse
sendOnSearchSuccess productCase transporterOrg productInstance = do
  let piStatus = productInstance ^. #_status
  let productInstances =
        case piStatus of
          ProductInstance.OUTOFSTOCK -> []
          _ -> [productInstance]
  onSearchPayload <- mkOnSearchPayload productCase productInstances transporterOrg
  let bppShortId = _getShortOrganizationId $ transporterOrg ^. #_shortId
  Gateway.onSearch onSearchPayload bppShortId

mkOnSearchPayload :: Case.Case -> [ProductInstance.ProductInstance] -> Org.Organization -> Flow API.OnSearchReq
mkOnSearchPayload productCase productInstances transporterOrg = do
  currTime <- getCurrTime
  appEnv <- ask
  let context =
        Context.Context
          { _domain = Domain.MOBILITY,
            _country = Just "IND",
            _city = Nothing,
            _action = "on_search",
            _core_version = Just "0.8.2",
            _domain_version = Just "0.8.2",
            _transaction_id = last $ T.split (== '_') $ productCase ^. #_shortId,
            _message_id = productCase ^. #_shortId,
            _bap_uri = Nothing,
            _bpp_uri = Just $ BP.makeBppUrl transporterOrg $ nwAddress appEnv,
            _timestamp = currTime,
            _ttl = Nothing
          }
  piCount <- MPI.getCountByStatus (_getOrganizationId $ transporterOrg ^. #_id) Case.RIDEORDER
  let stats = mkProviderStats piCount
  let provider = mkProviderInfo transporterOrg stats
  catalog <- GT.mkCatalog productCase productInstances provider
  return
    Callback.CallbackReq
      { context,
        contents = Right $ API.OnSearchServices catalog
      }

mkProviderInfo :: Org.Organization -> APICase.ProviderStats -> APICase.ProviderInfo
mkProviderInfo org stats =
  APICase.ProviderInfo
    { _id = _getOrganizationId $ org ^. #_id,
      _name = org ^. #_name,
      _stats = encodeToText stats,
      _contacts = fromMaybe "" (org ^. #_mobileNumber)
    }

mkProviderStats :: [(ProductInstance.ProductInstanceStatus, Int)] -> APICase.ProviderStats
mkProviderStats piCount =
  APICase.ProviderStats
    { _completed = List.lookup ProductInstance.COMPLETED piCount,
      _inprogress = List.lookup ProductInstance.INPROGRESS piCount,
      _confirmed = List.lookup ProductInstance.CONFIRMED piCount
    }
