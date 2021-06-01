{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.Search (search) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Callback as Callback
import qualified Beckn.Types.Core.API.Search as API
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Domain as Domain
import qualified Beckn.Types.Core.Error as Core
import qualified Beckn.Types.Core.Tag as Tag
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Stop as Stop
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified ExternalAPI.Transform as ExternalAPITransform
import qualified Models.ProductInstance as MPI
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
import Utils.Common

search :: Id Org.Organization -> Org.Organization -> API.SearchReq -> FlowHandler AckResponse
search transporterId bapOrg req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    let context = req ^. #context
    BP.validateContext "search" context
    uuid <- L.generateGUID
    transporter <- Org.findOrganizationById transporterId
    when (transporter ^. #_enabled) $ do
      let intent = req ^. #message . #intent
      now <- getCurrentTime
      let pickup = head $ intent ^. #_pickups
      let dropOff = head $ intent ^. #_drops
      let startTime = pickup ^. #_departure_time . #_est
      validity <- getValidTime now startTime
      fromLocation <- mkFromStop now pickup
      toLocation <- mkFromStop now dropOff
      let bapOrgId = bapOrg ^. #_id
      bapCallbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
      deadDistance <- calculateDeadDistance transporter fromLocation
      let productCase = mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance
      DB.runSqlDBTransaction $ do
        Loc.create fromLocation
        Loc.create toLocation
        QCase.create productCase
      fork "OnSearchCallback" $ onSearchCallback bapCallbackUrl productCase transporter fromLocation toLocation
    return Ack

mkFromStop :: UTCTime -> Stop.Stop -> Flow Location.Location
mkFromStop now stop = do
  let loc = stop ^. #_location
  let mgps = loc ^. #_gps
  let maddress = loc ^. #_address
  uuid <- Id <$> L.generateGUID
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
        _point = Location.Point,
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

mkCase :: API.SearchReq -> Text -> UTCTime -> UTCTime -> UTCTime -> Location.Location -> Location.Location -> Id Org.Organization -> Id Org.Organization -> Maybe Float -> Case.Case
mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance = do
  let intent = req ^. #message . #intent
  let distance = Tag._value <$> find (\x -> x ^. #_key == "distance") (fromMaybe [] $ intent ^. #_tags)
  let tId = getId transporterId
  let bapId = getId bapOrgId
  Case.Case
    { _id = Id uuid,
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
      _fromLocationId = getId $ fromLocation ^. #_id,
      _toLocationId = getId $ toLocation ^. #_id,
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
    orgLocId <- Id <$> organization ^. #_locationId & fromMaybeM (OrgFieldNotPresent "location_id")
    mbOrgLocation <- Loc.findLocationById orgLocId
    case mbOrgLocation of
      Nothing -> throwError LocationNotFound
      Just orgLocation -> Location.calculateDistance orgLocation fromLocation
  case eres of
    Left err -> do
      logTagWarning "calculateDeadDistance" $ "Failed to calculate distance. Reason: " +|| err ||+ ""
      pure Nothing
    Right mDistance -> return mDistance

onSearchCallback :: BaseUrl -> Case.Case -> Org.Organization -> Location.Location -> Location.Location -> Flow ()
onSearchCallback bapUri productCase transporter fromLocation toLocation = do
  let transporterId = transporter ^. #_id
  result <- runSafeFlow $ do
    vehicleVariant :: Vehicle.Variant <-
      (productCase ^. #_udf1 >>= readMaybe . T.unpack)
        & fromMaybeM (CaseFieldNotPresent "udf1")
    pool <-
      Person.calculateDriverPool (fromLocation ^. #_id) transporterId vehicleVariant
    logTagInfo "OnSearchCallback" $
      "Calculated Driver Pool for organization " +|| getId transporterId ||+ " with drivers " +| T.intercalate ", " (getId <$> pool) |+ ""
    let piStatus =
          if null pool
            then ProductInstance.OUTOFSTOCK
            else ProductInstance.INSTOCK
    price <-
      if null pool
        then return Nothing
        else Just <$> calculateFare transporterId vehicleVariant fromLocation toLocation (productCase ^. #_startTime) (productCase ^. #_udf5)
    prodInst <- mkProductInstance productCase price piStatus transporterId
    let caseStatus ProductInstance.INSTOCK = Case.CONFIRMED
        caseStatus _ = Case.CLOSED
    DB.runSqlDBTransaction $ do
      ProductInstance.create prodInst
      QCase.updateStatus (productCase ^. #_id) (caseStatus $ prodInst ^. #_status)
    pure prodInst
  case result of
    Right prodInst -> do
      let productStatus = prodInst ^. #_status
      logTagInfo "OnSearchCallback" $
        "Sending on_search callback with status " +|| productStatus ||+ " for product " +|| prodInst ^. #_id ||+ ""
      void $ sendOnSearchSuccess bapUri productCase transporter prodInst
    Left err -> do
      logTagError "OnSearchCallback" $ "Error happened when sending on_search request. Error: " +|| err ||+ ""
      void $ sendOnSearchFailed bapUri productCase transporter err

mkProductInstance :: Case.Case -> Maybe Amount -> ProductInstance.ProductInstanceStatus -> Id Org.Organization -> Flow ProductInstance.ProductInstance
mkProductInstance productCase price status transporterId = do
  productInstanceId <- Id <$> L.generateGUID
  now <- getCurrentTime
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
            _price = price,
            _status = status,
            _startTime = productCase ^. #_startTime,
            _endTime = productCase ^. #_endTime,
            _validTill = productCase ^. #_validTill,
            _fromLocation = Just $ productCase ^. #_fromLocationId,
            _toLocation = Just $ productCase ^. #_toLocationId,
            _organizationId = getId transporterId,
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
  pure productInstance

sendOnSearchFailed :: BaseUrl -> Case.Case -> Org.Organization -> Text -> Flow AckResponse
sendOnSearchFailed bapUri productCase transporterOrg err = do
  appEnv <- ask
  currTime <- getCurrentTime
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
            _bap_uri = Just bapUri,
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
                  { _type = Core.DOMAIN_ERROR,
                    _code = err,
                    _path = Nothing,
                    _message = Nothing
                  }
          }
  let bppShortId = getShortId $ transporterOrg ^. #_shortId
  ExternalAPI.onSearch payload bppShortId

sendOnSearchSuccess :: BaseUrl -> Case.Case -> Org.Organization -> ProductInstance.ProductInstance -> Flow AckResponse
sendOnSearchSuccess bapUri productCase transporterOrg productInstance = do
  let piStatus = productInstance ^. #_status
  let productInstances =
        case piStatus of
          ProductInstance.OUTOFSTOCK -> []
          _ -> [productInstance]
  onSearchPayload <- mkOnSearchPayload bapUri productCase productInstances transporterOrg
  let bppShortId = getShortId $ transporterOrg ^. #_shortId
  ExternalAPI.onSearch onSearchPayload bppShortId

mkOnSearchPayload :: BaseUrl -> Case.Case -> [ProductInstance.ProductInstance] -> Org.Organization -> Flow API.OnSearchReq
mkOnSearchPayload bapUri productCase productInstances transporterOrg = do
  currTime <- getCurrentTime
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
            _bap_uri = Just bapUri,
            _bpp_uri = Just $ BP.makeBppUrl transporterOrg $ nwAddress appEnv,
            _timestamp = currTime,
            _ttl = Nothing
          }
  piCount <- MPI.getCountByStatus (getId $ transporterOrg ^. #_id) Case.RIDEORDER
  let stats = mkProviderStats piCount
  let provider = mkProviderInfo transporterOrg stats
  catalog <- ExternalAPITransform.mkCatalog productCase productInstances provider
  return
    Callback.CallbackReq
      { context,
        contents = Right $ API.OnSearchServices catalog
      }

mkProviderInfo :: Org.Organization -> APICase.ProviderStats -> APICase.ProviderInfo
mkProviderInfo org stats =
  APICase.ProviderInfo
    { _id = getId $ org ^. #_id,
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
