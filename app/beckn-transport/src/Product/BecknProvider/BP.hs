{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.BP where

import App.Types
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Location as BL
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Service
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle as BVehicle
import Beckn.Types.Storage.Case as SC
import Beckn.Types.Storage.Location as SL
import Beckn.Types.Storage.Organization as Org
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.Products as Product
import Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Accessor as Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Time
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import External.Gateway.Flow as Gateway
import External.Gateway.Transform as GT
import Servant
import Storage.Queries.Case as Case
import Storage.Queries.Location as Loc
import Storage.Queries.Organization as Org
import Storage.Queries.Person as Person
import Storage.Queries.ProductInstance as ProductInstance
import Storage.Queries.Products as Product
import Storage.Queries.Vehicle as Vehicle
import System.Environment
import qualified Test.RandomStrings as RS
import Types.Notification
import qualified Utils.Notifications as Notify

-- 1) Create Parent Case with Customer Request Details
-- 2) Notify all transporter using FCM
-- 3) Respond with Ack

search :: SearchReq -> FlowHandler AckResponse
search req = withFlowHandler $ do
  --TODO: Need to add authenticator
  let intent = req ^. #message
  uuid <- L.generateGUID
  currTime <- getCurrentTimeUTC
  validity <- getValidTime currTime (req ^. #message . #time)
  uuid1 <- L.generateGUID
  let fromLocation = mkFromLocation req uuid1 currTime $ req ^. #message . #origin
  uuid2 <- L.generateGUID
  let toLocation = mkFromLocation req uuid2 currTime $ req ^. #message . #destination
  Loc.create fromLocation
  Loc.create toLocation
  let c = mkCase req uuid currTime validity fromLocation toLocation
  Case.create c
  transporters <- listOrganizations Nothing Nothing [Org.TRANSPORTER] [Org.APPROVED]
  -- TODO : Fix show
  admins <-
    Person.findAllByOrgIds
      [Person.ADMIN]
      (_getOrganizationId . Org._id <$> transporters)
  Notify.notifyTransportersOnSearch c intent admins
  mkAckResponse uuid "search"

cancel :: CancelReq -> FlowHandler AckResponse
cancel req = withFlowHandler $ do
  --TODO: Need to add authenticator
  uuid <- L.generateGUID
  let prodInstId = req ^. #message . #id -- transporter search productInstId
  prodInst <- ProductInstance.findById (ProductInstanceId prodInstId)
  piList <- ProductInstance.findAllByParentId (Just $ prodInst ^. #_id)
  -- TODO: Should we check if all case's products were cancelled
  -- before cancelling a case?
  Case.updateStatusByIds (ProductInstance._caseId <$> piList) SC.CLOSED
  ProductInstance.updateStatusByIds (ProductInstance._id <$> piList) ProductInstance.CANCELLED
  notifyCancelToGateway prodInstId
  admins <-
    Person.findAllByOrgIds [Person.ADMIN] [ProductInstance._organizationId prodInst]
  case piList of
    [] -> pure ()
    pi : _ -> do
      c <- Case.findById $ ProductInstance._caseId pi
      Notify.notifyTransportersOnCancel c admins
  mkAckResponse uuid "cancel"

-- TODO: Move this to core Utils.hs
getValidTime :: LocalTime -> LocalTime -> Flow LocalTime
getValidTime now startTime = do
  caseExpiryEnv <- L.runIO $ lookupEnv "DEFAULT_CASE_EXPIRY"
  let caseExpiry = fromMaybe 7200 $ readMaybe =<< caseExpiryEnv
      minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffLocalTime` now
      defaultExpiry = fromInteger caseExpiry `addLocalTime` now
      validTill = addLocalTime (minimum [fromInteger caseExpiry, maximum [minExpiry, timeToRide]]) now
  pure validTill

mkFromLocation :: SearchReq -> Text -> LocalTime -> BL.Location -> SL.Location
mkFromLocation req uuid now loc =
  case loc ^. #_type of
    "gps" -> case loc ^. #_gps of
      Just (val :: GPS) -> mkLocationRecord uuid now SL.POINT (Just $ read $ T.unpack $ val ^. #lat) (Just $ read $ T.unpack $ val ^. #lon) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      _ -> undefined -- need to throw error
    "address" -> case loc ^. #_address of
      Just (val :: Address) -> mkLocationRecord uuid now SL.ADDRESS Nothing Nothing (Just $ val ^. #area) Nothing (Just $ val ^. #area) Nothing (Just $ val ^. #country) (Just $ val ^. #area_code) (Just $ (val ^. #door) <> (val ^. #building) <> (val ^. #street)) Nothing
      _ -> undefined -- need to throw error
    _ -> mkLocationRecord uuid now SL.POINT Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkLocationRecord ::
  Text ->
  LocalTime ->
  SL.LocationType ->
  Maybe Double ->
  Maybe Double ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  SL.Location
mkLocationRecord idr time typ lat lon ward dis city state country pincode address bound =
  SL.Location
    { _id = LocationId {_getLocationId = idr},
      _locationType = typ,
      _lat = lat,
      _long = lon,
      _ward = ward,
      _district = dis,
      _city = city,
      _state = state,
      _country = country,
      _pincode = pincode,
      _address = address,
      _bound = bound,
      _createdAt = time,
      _updatedAt = time
    }

mkCase :: SearchReq -> Text -> LocalTime -> LocalTime -> SL.Location -> SL.Location -> SC.Case
mkCase req uuid now validity fromLocation toLocation = do
  let intent = req ^. #message
  SC.Case
    { _id = CaseId {_getCaseId = uuid},
      _name = Nothing,
      _description = Just "Case to search for a Ride",
      _shortId = req ^. #context . #transaction_id,
      _industry = SC.MOBILITY,
      _type = RIDESEARCH,
      _exchangeType = FULFILLMENT,
      _status = NEW,
      _startTime = intent ^. #time,
      _endTime = Nothing,
      _validTill = validity,
      _provider = Nothing,
      _providerType = Nothing,
      _requestor = Nothing,
      _requestorType = Just CONSUMER,
      _parentCaseId = Nothing,
      _fromLocationId = fromLocation ^. #_id ^. #_getLocationId,
      _toLocationId = toLocation ^. #_id ^. #_getLocationId,
      _udf1 = Just $ intent ^. #vehicle . #variant,
      _udf2 = Just $ show $ intent ^. #payload . #travellers . #count,
      _udf3 = Nothing,
      _udf4 = Nothing,
      _udf5 = Nothing,
      _info = Nothing, --Just $ show $ req ^. #message
      _createdAt = now,
      _updatedAt = now
    }

confirm :: ConfirmReq -> FlowHandler AckResponse
confirm req = withFlowHandler $ do
  L.logInfo "confirm API Flow" "Reached"
  let prodInstId = ProductInstanceId $ head $ req ^. #message . #_selected_items
  let caseShortId = req ^. #context . #transaction_id -- change to message.transactionId
  search_case <- Case.findBySid caseShortId
  productInstance <- ProductInstance.findById prodInstId
  orderCase <- mkOrderCase search_case
  Case.create orderCase
  orderProductInstance <- mkOrderProductInstance (orderCase ^. #_id) productInstance
  ProductInstance.create orderProductInstance
  Case.updateStatus (orderCase ^. #_id) SC.INPROGRESS
  ProductInstance.updateStatusByIds [productInstance ^. #_id, orderProductInstance ^. #_id] ProductInstance.CONFIRMED
  --TODO: need to update other product status to VOID for this case
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  uuid <- L.generateGUID
  currTime <- getCurrentTimeUTC
  let trackerCase = mkTrackerCase search_case uuid currTime $ T.pack shortId
  Case.create trackerCase
  uuid1 <- L.generateGUID
  trackerProductInstance <- mkTrackerProductInstance uuid1 (trackerCase ^. #_id) productInstance currTime
  ProductInstance.create trackerProductInstance
  notifyGateway search_case prodInstId trackerCase
  admins <-
    Person.findAllByOrgIds [Person.ADMIN] [productInstance ^. #_organizationId]
  Notify.notifyTransportersOnConfirm search_case productInstance admins
  mkAckResponse uuid "confirm"

mkOrderCase :: SC.Case -> Flow SC.Case
mkOrderCase SC.Case {..} = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return $
    SC.Case
      { _id = id,
        _name = Nothing,
        _description = Just "Case to order a Ride",
        _shortId = shortId,
        _industry = SC.MOBILITY,
        _type = SC.RIDEORDER,
        _parentCaseId = Just _id,
        _createdAt = now,
        _updatedAt = now,
        ..
      }

mkOrderProductInstance :: CaseId -> ProductInstance -> Flow ProductInstance.ProductInstance
mkOrderProductInstance caseId prodInst = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return $
    ProductInstance.ProductInstance
      { _id = ProductInstanceId id,
        _caseId = caseId,
        _productId = prodInst ^. #_productId,
        _personId = Nothing,
        _entityType = ProductInstance.VEHICLE,
        _entityId = Nothing,
        _shortId = shortId,
        _quantity = 1,
        _price = prodInst ^. #_price,
        _organizationId = prodInst ^. #_organizationId,
        _fromLocation = prodInst ^. #_fromLocation,
        _toLocation = prodInst ^. #_toLocation,
        _startTime = prodInst ^. #_startTime,
        _endTime = prodInst ^. #_endTime,
        _validTill = prodInst ^. #_validTill,
        _parentId = Just (prodInst ^. #_id),
        _status = ProductInstance.INSTOCK,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now,
        _udf1 = prodInst ^. #_udf1,
        _udf2 = prodInst ^. #_udf2,
        _udf3 = prodInst ^. #_udf3,
        _udf4 = prodInst ^. #_udf4,
        _udf5 = prodInst ^. #_udf5
      }

-- TODO : Add notifying transporter admin with FCM

mkTrackerProductInstance :: Text -> CaseId -> ProductInstance -> LocalTime -> Flow ProductInstance.ProductInstance
mkTrackerProductInstance piId caseId prodInst currTime = do
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return $
    ProductInstance.ProductInstance
      { _id = ProductInstanceId piId,
        _caseId = caseId,
        _productId = prodInst ^. #_productId,
        _personId = Nothing,
        _shortId = shortId,
        _entityType = ProductInstance.VEHICLE,
        _parentId = Just (prodInst ^. #_id),
        _organizationId = prodInst ^. #_organizationId,
        _entityId = Nothing,
        _startTime = prodInst ^. #_startTime,
        _endTime = prodInst ^. #_endTime,
        _fromLocation = prodInst ^. #_fromLocation,
        _toLocation = prodInst ^. #_toLocation,
        _validTill = prodInst ^. #_validTill,
        _quantity = 1,
        _price = 0,
        _status = ProductInstance.INSTOCK,
        _info = Nothing,
        _createdAt = currTime,
        _updatedAt = currTime,
        _udf1 = prodInst ^. #_udf1,
        _udf2 = prodInst ^. #_udf2,
        _udf3 = prodInst ^. #_udf3,
        _udf4 = prodInst ^. #_udf4,
        _udf5 = prodInst ^. #_udf5
      }

mkTrackerCase :: SC.Case -> Text -> LocalTime -> Text -> SC.Case
mkTrackerCase case_ uuid now shortId =
  SC.Case
    { _id = CaseId {_getCaseId = uuid},
      _name = Nothing,
      _description = Just "Case to track a Ride",
      _shortId = shortId,
      _industry = SC.MOBILITY,
      _type = LOCATIONTRACKER,
      _exchangeType = FULFILLMENT,
      _status = NEW,
      _startTime = case_ ^. #_startTime, --TODO: should we make it startTime - 30 mins?
      _endTime = Nothing,
      _validTill = case_ ^. #_validTill,
      _provider = Nothing,
      _providerType = Nothing, --TODO: Ensure to update when getting Driver Info
      _requestor = Nothing,
      _requestorType = Just CONSUMER,
      _parentCaseId = Just $ case_ ^. #_id,
      _fromLocationId = case_ ^. #_fromLocationId,
      _toLocationId = case_ ^. #_toLocationId,
      _udf1 = Nothing,
      _udf2 = Nothing,
      _udf3 = Nothing,
      _udf4 = Nothing,
      _udf5 = Nothing,
      _info = Nothing,
      _createdAt = now,
      _updatedAt = now
    }

notifyGateway :: Case -> ProductInstanceId -> Case -> Flow ()
notifyGateway c prodInstId trackerCase = do
  L.logInfo "notifyGateway" $ show c
  pis <- ProductInstance.findAllByCaseId (c ^. #_id)
  onConfirmPayload <- mkOnConfirmPayload c pis pis trackerCase
  L.logInfo "notifyGateway onConfirm Request Payload" $ show onConfirmPayload
  Gateway.onConfirm onConfirmPayload
  return ()

mkOnConfirmPayload :: Case -> [ProductInstance] -> [ProductInstance] -> Case -> Flow OnConfirmReq
mkOnConfirmPayload c pis allPis trackerCase = do
  currTime <- getCurrentTimeUTC
  let context =
        Context
          { domain = "MOBILITY",
            action = "CONFIRM",
            version = Just "0.1",
            transaction_id = c ^. #_shortId, -- TODO : What should be the txnId
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  trip <- mkTrip trackerCase
  service <- GT.mkServiceOffer c pis allPis (Just trip) Nothing
  return
    OnConfirmReq
      { context,
        message = service
      }

serviceStatus :: StatusReq -> FlowHandler StatusRes
serviceStatus req = withFlowHandler $ do
  L.logInfo "serviceStatus API Flow" $ show req
  let caseId = req ^. #message . #id
  c <- Case.findById (CaseId caseId)
  pis <- ProductInstance.findAllByCaseId (c ^. #_id)
  --TODO : use forkFlow to notify gateway
  trackerCase <- Case.findByParentCaseIdAndType (CaseId caseId) SC.LOCATIONTRACKER
  notifyServiceStatusToGateway c pis pis trackerCase
  uuid <- L.generateGUID
  mkAckResponse uuid "track"

notifyServiceStatusToGateway :: Case -> [ProductInstance] -> [ProductInstance] -> Maybe Case -> Flow ()
notifyServiceStatusToGateway c pis allpis trackerCase = do
  onServiceStatusPayload <- mkOnServiceStatusPayload c pis allpis trackerCase
  L.logInfo "notifyServiceStatusToGateway Request" $ show onServiceStatusPayload
  Gateway.onStatus onServiceStatusPayload
  return ()

mkOnServiceStatusPayload :: Case -> [ProductInstance] -> [ProductInstance] -> Maybe Case -> Flow OnStatusReq
mkOnServiceStatusPayload c pis allPis trackerCase = do
  currTime <- getCurrentTimeUTC
  let context =
        Context
          { domain = "MOBILITY",
            action = "on_status",
            version = Just "0.1",
            transaction_id = c ^. #_shortId,
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  trip <- mapM mkTrip trackerCase
  service <- GT.mkServiceOffer c pis allPis trip Nothing
  return
    OnStatusReq
      { context,
        message = service
      }

trackTrip :: TrackTripReq -> FlowHandler TrackTripRes
trackTrip req = withFlowHandler $ do
  L.logInfo "track trip API Flow" $ show req
  let tripId = req ^. #message . #id
  case_ <- Case.findById (CaseId tripId)
  case case_ ^. #_parentCaseId of
    Just parentCaseId -> do
      parentCase <- Case.findById parentCaseId
      --TODO : use forkFlow to notify gateway
      notifyTripUrlToGateway case_ parentCase
      uuid <- L.generateGUID
      mkAckResponse uuid "track"
    Nothing -> L.throwException $ err400 {errBody = "Case does not have an associated parent case"}

notifyTripUrlToGateway :: Case -> Case -> Flow ()
notifyTripUrlToGateway case_ parentCase = do
  onTrackTripPayload <- mkOnTrackTripPayload case_ parentCase
  L.logInfo "notifyTripUrlToGateway Request" $ show onTrackTripPayload
  Gateway.onTrackTrip onTrackTripPayload
  return ()

notifyCancelToGateway :: Text -> Flow ()
notifyCancelToGateway prodInstId = do
  onCancelPayload <- mkCancelRidePayload prodInstId
  L.logInfo "notifyGateway Request" $ show onCancelPayload
  Gateway.onCancel onCancelPayload
  return ()

mkOnTrackTripPayload :: Case -> Case -> Flow OnTrackTripReq
mkOnTrackTripPayload case_ pCase = do
  currTime <- getCurrentTimeUTC
  let context =
        Context
          { domain = "MOBILITY",
            action = "on_track",
            version = Just "0.1",
            transaction_id = pCase ^. #_shortId,
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  let data_url = GT.baseTrackingUrl <> "/" <> _getCaseId (case_ ^. #_id)
  let embed_url = GT.baseTrackingUrl <> "/" <> _getCaseId (case_ ^. #_id) <> "/embed"
  trip <- mkTrip case_
  let trackingUrl = GT.mkTracking "PULL" data_url embed_url
  let tracker = Tracker trip (Just trackingUrl)
  return
    OnTrackTripReq
      { context,
        message = tracker
      }

mkTrip :: Case -> Flow Trip
mkTrip c = do
  let data_url = GT.baseTrackingUrl <> "/" <> _getCaseId (c ^. #_id)
      embed_url = GT.baseTrackingUrl <> "/" <> _getCaseId (c ^. #_id) <> "/embed"
  pi <- ProductInstance.findByCaseId $ c ^. #_id
  driver <- mapM mkDriverInfo $ pi ^. #_personId
  vehicle <- join <$> mapM mkVehicleInfo (pi ^. #_entityId)
  -- let vehicleInfo = decodeMTypeFromText <$> prod ^. #_info
  L.logInfo "vehicle" $ show vehicle
  return $
    Trip
      { id = _getCaseId $ c ^. #_id,
        vehicle = vehicle,
        driver =
          TripDriver
            { persona = driver,
              rating = Nothing
            },
        travellers = [],
        tracking = GT.mkTracking "PULL" data_url embed_url,
        corridor_type = "ON-DEMAND",
        state = "", -- TODO: need to take it from product
        fare = Nothing, -- TODO: need to take it from product
        route = Nothing
      }

mkDriverInfo :: PersonId -> Flow Driver
mkDriverInfo driverId = do
  person <- Person.findPersonById driverId
  return $ GT.mkDriverObj person

mkVehicleInfo :: Text -> Flow (Maybe BVehicle.Vehicle)
mkVehicleInfo vehicleId = do
  vehicle <- Vehicle.findVehicleById (VehicleId vehicleId)
  return $ GT.mkVehicleObj <$> vehicle

mkCancelRidePayload :: Text -> Flow OnCancelReq
mkCancelRidePayload prodInstId = do
  currTime <- getCurrentTimeUTC
  let context =
        Context
          { domain = "MOBILITY",
            action = "on_cancel",
            version = Just "0.7.1",
            transaction_id = "",
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  tripObj <- mkCancelTripObj prodInstId
  return
    OnCancelReq
      { context,
        message = tripObj
      }

mkCancelTripObj :: Text -> Flow Trip
mkCancelTripObj prodInstId = do
  productInstance <- ProductInstance.findById (ProductInstanceId prodInstId)
  driver <- mapM mkDriverInfo $ productInstance ^. #_personId
  vehicle <- join <$> mapM mkVehicleInfo (productInstance ^. #_entityId)
  return $
    Trip
      { id = prodInstId,
        vehicle = vehicle,
        driver =
          TripDriver
            { persona = driver,
              rating = Nothing
            },
        travellers = [],
        tracking = Tracking "" Nothing,
        corridor_type = "",
        state = show $ productInstance ^. #_status,
        fare = Just $ GT.mkPrice productInstance,
        route = Nothing
      }
