{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.BecknProvider.BP where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.API.Callback
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.API.Update
import Beckn.Types.App as TA
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain as Domain
import Beckn.Types.Core.ItemQuantity
import Beckn.Types.Core.Order
import qualified Beckn.Types.Core.Tag as Tag
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Payload
import Beckn.Types.Mobility.Stop as BS
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle as BVehicle
import Beckn.Types.Storage.Case as SC
import Beckn.Types.Storage.Location as SL
import Beckn.Types.Storage.Organization as Org
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
import qualified Data.Text as T
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import External.Gateway.Flow as Gateway
import External.Gateway.Transform as GT
import Models.Case as Case
import Servant hiding (Context)
import Storage.Queries.Location as Loc
import Storage.Queries.Organization as Org
import Storage.Queries.Person as Person
import Storage.Queries.ProductInstance as ProductInstance
import Storage.Queries.Vehicle as Vehicle
import qualified Test.RandomStrings as RS
import qualified Utils.Notifications as Notify

-- 1) Create Parent Case with Customer Request Details
-- 2) Notify all transporter using FCM
-- 3) Respond with Ack

search :: Organization -> SearchReq -> FlowHandler AckResponse
search organization req = withFlowHandler $ do
  --TODO: Need to add authenticator
  validateContext "search" $ req ^. #context
  uuid <- L.generateGUID
  when (organization ^. #_enabled) $ do
    let intent = req ^. #message . #intent
    currTime <- getCurrTime
    let pickup = head $ intent ^. #_pickups
        dropOff = head $ intent ^. #_drops
        startTime = pickup ^. #_departure_time . #_est
    validity <- getValidTime currTime startTime
    uuid1 <- L.generateGUID
    let fromLocation = mkFromStop req uuid1 currTime pickup
    uuid2 <- L.generateGUID
    let toLocation = mkFromStop req uuid2 currTime dropOff
    Loc.create fromLocation
    Loc.create toLocation
    let c = mkCase req uuid currTime validity startTime fromLocation toLocation organization
    Case.create c
    -- TODO : Fix show
    admins <-
      Person.findAllByOrgIds
        [Person.ADMIN]
        [_getOrganizationId $ organization ^. #_id]
    Notify.notifyTransportersOnSearch c intent admins
  mkAckResponse uuid "search"

cancel :: CancelReq -> FlowHandler AckResponse
cancel req = withFlowHandler $ do
  --TODO: Need to add authenticator
  validateContext "cancel" $ req ^. #context
  uuid <- L.generateGUID
  let cancelType = req ^. #message . #order . #cancellation_reason_id
  case cancelType of
    Just "CASE" -> do
      let caseShortId = req ^. #message . #order . #id --  transporter search productInstId
      searchCase <- Case.findBySid caseShortId
      Case.updateStatus (searchCase ^. #_id) SC.COMPLETED
    _ -> do
      let prodInstId = req ^. #message . #order . #id -- transporter search productInstId
      prodInst <- ProductInstance.findById (ProductInstanceId prodInstId)
      piList <- ProductInstance.findAllByParentId (Just $ prodInst ^. #_id)
      -- TODO: Should we check if all case's products were cancelled
      -- before cancelling a case?
      Case.updateStatusByIds (ProductInstance._caseId <$> piList) SC.CLOSED
      Case.updateStatus (prodInst ^. #_caseId) SC.COMPLETED
      case piList of
        [] -> return ()
        _ -> do
          trackerPi <- ProductInstance.findByIdType (ProductInstance._id <$> piList) SC.LOCATIONTRACKER
          orderPi <- ProductInstance.findByIdType (ProductInstance._id <$> piList) SC.RIDEORDER
          _ <- ProductInstance.updateStatus (ProductInstance._id trackerPi) ProductInstance.COMPLETED
          _ <- ProductInstance.updateStatus (ProductInstance._id orderPi) ProductInstance.CANCELLED
          return ()
      _ <- ProductInstance.updateStatus (ProductInstanceId prodInstId) ProductInstance.CANCELLED
      org <- Org.findOrganizationById $ OrganizationId $ ProductInstance._organizationId prodInst
      notifyCancelToGateway prodInstId
      admins <-
        if org ^. #_enabled
          then Person.findAllByOrgIds [Person.ADMIN] [ProductInstance._organizationId prodInst]
          else return []
      case piList of
        [] -> pure ()
        prdInst : _ -> do
          c <- Case.findById $ ProductInstance._caseId prdInst
          case prdInst ^. #_personId of
            Just driverId -> do
              driver <- Person.findPersonById driverId
              Notify.notifyTransportersOnCancel c (driver : admins)
            Nothing -> Notify.notifyTransportersOnCancel c admins
  mkAckResponse uuid "cancel"

-- TODO: Move this to core Utils.hs
getValidTime :: UTCTime -> UTCTime -> Flow UTCTime
getValidTime now startTime = do
  caseExpiry_ <- fromMaybe 7200 . caseExpiry <$> ask
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

mkFromStop :: SearchReq -> Text -> UTCTime -> BS.Stop -> SL.Location
mkFromStop _req uuid now stop =
  let loc = stop ^. #_location
      mgps = loc ^. #_gps
      maddress = loc ^. #_address
   in SL.Location
        { _id = LocationId uuid,
          _locationType = SL.POINT,
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

mkCase :: SearchReq -> Text -> UTCTime -> UTCTime -> UTCTime -> SL.Location -> SL.Location -> Organization -> SC.Case
mkCase req uuid now validity startTime fromLocation toLocation org = do
  let intent = req ^. #message . #intent
  let distance = Tag._value <$> find (\x -> x ^. #_key == "distance") (fromMaybe [] $ intent ^. #_tags)
  let orgId = _getOrganizationId $ org ^. #_id
  SC.Case
    { _id = CaseId {_getCaseId = uuid},
      _name = Nothing,
      _description = Just "Case to search for a Ride",
      _shortId = orgId <> "_" <> req ^. #context . #_transaction_id,
      _industry = SC.MOBILITY,
      _type = RIDESEARCH,
      _exchangeType = FULFILLMENT,
      _status = NEW,
      _startTime = startTime,
      _endTime = Nothing,
      _validTill = validity,
      _provider = Just orgId,
      _providerType = Nothing,
      _requestor = Nothing,
      _requestorType = Just CONSUMER,
      _parentCaseId = Nothing,
      _fromLocationId = TA._getLocationId $ fromLocation ^. #_id,
      _toLocationId = TA._getLocationId $ toLocation ^. #_id,
      _udf1 = Just $ intent ^. #_vehicle . #variant,
      _udf2 = Just $ show $ length $ intent ^. #_payload . #_travellers,
      _udf3 = Nothing,
      _udf4 = Nothing,
      _udf5 = distance,
      _info = Nothing, --Just $ show $ req ^. #message
      _createdAt = now,
      _updatedAt = now
    }

confirm :: ConfirmReq -> FlowHandler AckResponse
confirm req = withFlowHandler $ do
  L.logInfo @Text "confirm API Flow" "Reached"
  validateContext "confirm" $ req ^. #context
  let prodInstId = ProductInstanceId $ req ^. #message . #order . #_id
  let caseShortId = req ^. #context . #_transaction_id -- change to message.transactionId
  searchCase <- Case.findBySid caseShortId
  productInstance <- ProductInstance.findById prodInstId
  orderCase <- mkOrderCase searchCase
  Case.create orderCase
  orderProductInstance <- mkOrderProductInstance (orderCase ^. #_id) productInstance
  ProductInstance.create orderProductInstance
  Case.updateStatus (orderCase ^. #_id) SC.INPROGRESS
  _ <- ProductInstance.updateStatus (productInstance ^. #_id) ProductInstance.CONFIRMED
  --TODO: need to update other product status to VOID for this case
  (currTime, uuid, shortId) <- getIdShortIdAndTime
  let trackerCase = mkTrackerCase searchCase uuid currTime shortId
  Case.create trackerCase
  uuid1 <- L.generateGUID
  trackerProductInstance <- mkTrackerProductInstance uuid1 (trackerCase ^. #_id) productInstance currTime
  ProductInstance.create trackerProductInstance
  Case.updateStatus (searchCase ^. #_id) SC.COMPLETED
  notifyGateway searchCase productInstance trackerCase
  org <- Org.findOrganizationById $ OrganizationId $ productInstance ^. #_organizationId
  admins <-
    if org ^. #_enabled
      then Person.findAllByOrgIds [Person.ADMIN] [productInstance ^. #_organizationId]
      else return []
  Notify.notifyTransportersOnConfirm searchCase productInstance admins
  mkAckResponse uuid "confirm"

mkOrderCase :: SC.Case -> Flow SC.Case
mkOrderCase SC.Case {..} = do
  (now, cid, shortId) <- getIdShortIdAndTime
  return $
    SC.Case
      { _id = cid,
        _name = Nothing,
        _description = Just "Case to order a Ride",
        _shortId = shortId,
        _industry = SC.MOBILITY,
        _type = SC.RIDEORDER,
        _parentCaseId = Just _id,
        _status = SC.INPROGRESS,
        _fromLocationId = _fromLocationId,
        _toLocationId = _toLocationId,
        _createdAt = now,
        _updatedAt = now,
        ..
      }

mkOrderProductInstance :: CaseId -> ProductInstance -> Flow ProductInstance.ProductInstance
mkOrderProductInstance caseId prodInst = do
  (now, pid, shortId) <- getIdShortIdAndTime
  return $
    ProductInstance.ProductInstance
      { _id = ProductInstanceId pid,
        _caseId = caseId,
        _productId = prodInst ^. #_productId,
        _personId = Nothing,
        _entityType = ProductInstance.VEHICLE,
        _entityId = Nothing,
        _shortId = shortId,
        _quantity = 1,
        _price = prodInst ^. #_price,
        _type = SC.RIDEORDER,
        _organizationId = prodInst ^. #_organizationId,
        _fromLocation = prodInst ^. #_fromLocation,
        _toLocation = prodInst ^. #_toLocation,
        _startTime = prodInst ^. #_startTime,
        _endTime = prodInst ^. #_endTime,
        _validTill = prodInst ^. #_validTill,
        _parentId = Just (prodInst ^. #_id),
        _status = ProductInstance.CONFIRMED,
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

mkTrackerProductInstance :: Text -> CaseId -> ProductInstance -> UTCTime -> Flow ProductInstance.ProductInstance
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
        _type = SC.LOCATIONTRACKER,
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

mkTrackerCase :: SC.Case -> Text -> UTCTime -> Text -> SC.Case
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

notifyGateway :: Case -> ProductInstance -> Case -> Flow ()
notifyGateway c prodInst trackerCase = do
  L.logInfo @Text "notifyGateway" $ show c
  allPis <- ProductInstance.findAllByCaseId (c ^. #_id)
  onConfirmPayload <- mkOnConfirmPayload c [prodInst] allPis trackerCase
  L.logInfo @Text "notifyGateway onConfirm Request Payload" $ show onConfirmPayload
  _ <- Gateway.onConfirm onConfirmPayload
  return ()

mkContext :: Text -> Text -> Flow Context
mkContext action tId = do
  currTime <- getCurrTime
  return
    Context
      { _domain = Domain.MOBILITY,
        _action = action,
        _country = Just "IND",
        _city = Nothing,
        _core_version = Just "0.8.0",
        _domain_version = Just "0.8.2",
        _transaction_id = tId,
        _message_id = tId,
        _bap_uri = Nothing,
        _bpp_uri = Nothing,
        _timestamp = currTime
      }

mkOnConfirmPayload :: Case -> [ProductInstance] -> [ProductInstance] -> Case -> Flow OnConfirmReq
mkOnConfirmPayload c pis _allPis trackerCase = do
  context <- mkContext "on_confirm" $ c ^. #_shortId -- TODO : What should be the txnId
  trip <- mkTrip trackerCase
  order <- GT.mkOrder c (head pis) (Just trip)
  return
    CallbackReq
      { context,
        contents = Right $ ConfirmOrder order
      }

notifyOrgCountToGateway :: Text -> Integer -> Flow ()
notifyOrgCountToGateway caseSid count = do
  onStatusOrgCountPayload <- mkOrgCountPayload caseSid count
  _ <- Gateway.onStatus onStatusOrgCountPayload
  return ()

mkOrgCountPayload :: Text -> Integer -> Flow OnStatusReq
mkOrgCountPayload caseSid count = do
  context <- mkContext "on_status" caseSid
  order <- mkOrgCountRes
  let onStatusMessage = OnStatusReqMessage order
  return $
    CallbackReq
      { context = context,
        contents = Right onStatusMessage
      }
  where
    mkOrgCountRes = do
      now <- getCurrTime
      return $
        Order
          { _id = caseSid,
            _state = "ORG_COUNT",
            _items = [OrderItem caseSid (Just mkItemQuantityRes)],
            _created_at = now,
            _updated_at = now,
            _billing = Nothing,
            _payment = Nothing,
            _update_action = Nothing,
            _quotation = Nothing
          }
    mkItemQuantityRes =
      ItemQuantity
        { _allocated = Nothing,
          _available = Just $ Quantity count example,
          _maximum = Nothing,
          _minimum = Nothing,
          _selected = Nothing
        }

serviceStatus :: StatusReq -> FlowHandler StatusRes
serviceStatus req = withFlowHandler $ do
  L.logInfo @Text "serviceStatus API Flow" $ show req
  --  let caseSid = req ^. #message . #service . #id
  --  c <- Case.findBySid caseSid
  let piId = req ^. #message . #order . #id -- transporter search product instance id
  trackerPi <- ProductInstance.findByParentIdType (Just $ ProductInstanceId piId) SC.LOCATIONTRACKER
  --TODO : use forkFlow to notify gateway
  notifyServiceStatusToGateway piId trackerPi
  uuid <- L.generateGUID
  mkAckResponse uuid "status"

notifyServiceStatusToGateway :: Text -> ProductInstance -> Flow ()
notifyServiceStatusToGateway piId trackerPi = do
  onServiceStatusPayload <- mkOnServiceStatusPayload piId trackerPi
  L.logInfo @Text "notifyServiceStatusToGateway Request" $ show onServiceStatusPayload
  _ <- Gateway.onStatus onServiceStatusPayload
  return ()

mkOnServiceStatusPayload :: Text -> ProductInstance -> Flow OnStatusReq
mkOnServiceStatusPayload piId trackerPi = do
  context <- mkContext "on_status" "" -- FIXME: transaction id?
  order <- mkOrderRes piId (_getProductsId $ trackerPi ^. #_productId) (show $ trackerPi ^. #_status)
  let onStatusMessage = OnStatusReqMessage order
  return $
    CallbackReq
      { context = context,
        contents = Right onStatusMessage
      }
  where
    mkOrderRes prodInstId productId status = do
      now <- getCurrTime
      return $
        Order
          { _id = prodInstId,
            _state = T.pack status,
            _items = [OrderItem productId Nothing],
            _created_at = now,
            _updated_at = now,
            _billing = Nothing,
            _payment = Nothing,
            _update_action = Nothing,
            _quotation = Nothing
          }

trackTrip :: TrackTripReq -> FlowHandler TrackTripRes
trackTrip req = withFlowHandler $ do
  L.logInfo @Text "track trip API Flow" $ show req
  validateContext "track" $ req ^. #context
  let tripId = req ^. #message . #order_id
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
  L.logInfo @Text "notifyTripUrlToGateway Request" $ show onTrackTripPayload
  _ <- Gateway.onTrackTrip onTrackTripPayload
  return ()

notifyTripInfoToGateway :: ProductInstance -> Case -> Case -> Flow ()
notifyTripInfoToGateway prodInst trackerCase parentCase = do
  onUpdatePayload <- mkOnUpdatePayload prodInst trackerCase parentCase
  L.logInfo @Text "notifyTripInfoToGateway Request" $ show onUpdatePayload
  _ <- Gateway.onUpdate onUpdatePayload
  return ()

notifyCancelToGateway :: Text -> Flow ()
notifyCancelToGateway prodInstId = do
  onCancelPayload <- mkCancelRidePayload prodInstId -- search product instance id
  L.logInfo @Text "notifyGateway Request" $ show onCancelPayload
  _ <- Gateway.onCancel onCancelPayload
  return ()

mkOnTrackTripPayload :: Case -> Case -> Flow OnTrackTripReq
mkOnTrackTripPayload trackerCase parentCase = do
  context <- mkContext "on_track" $ parentCase ^. #_shortId
  let data_url = GT.baseTrackingUrl <> "/" <> _getCaseId (trackerCase ^. #_id)
  let tracking = GT.mkTracking "PULL" data_url
  return
    CallbackReq
      { context,
        contents = Right $ OnTrackReqMessage (Just tracking)
      }

mkTrip :: Case -> Flow Trip
mkTrip c = do
  prodInst <- ProductInstance.findByCaseId $ c ^. #_id
  driver <- mapM mkDriverInfo $ prodInst ^. #_personId
  vehicle <- join <$> mapM mkVehicleInfo (prodInst ^. #_entityId)
  L.logInfo @Text "vehicle" $ show vehicle
  return $
    Trip
      { id = _getCaseId $ c ^. #_id,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = Nothing,
        route = Nothing
      }

mkOnUpdatePayload :: ProductInstance -> Case -> Case -> Flow OnUpdateReq
mkOnUpdatePayload prodInst case_ pCase = do
  context <- mkContext "on_update" $ _getProductInstanceId $ prodInst ^. #_id
  trip <- mkTrip case_
  order <- GT.mkOrder pCase prodInst (Just trip)
  return
    CallbackReq
      { context,
        contents = Right $ OnUpdateOrder order
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
  context <- mkContext "on_cancel" "" -- FIXME: transaction id?
  tripObj <- mkCancelTripObj prodInstId
  return
    CallbackReq
      { context,
        contents = Right tripObj
      }

mkCancelTripObj :: Text -> Flow Trip
mkCancelTripObj prodInstId = do
  productInstance <- ProductInstance.findById (ProductInstanceId prodInstId)
  driver <- mapM mkDriverInfo $ productInstance ^. #_personId
  vehicle <- join <$> mapM mkVehicleInfo (productInstance ^. #_entityId)
  return $
    Trip
      { id = prodInstId,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = Just $ GT.mkPrice productInstance,
        route = Nothing
      }

getIdShortIdAndTime :: GuidLike b => Flow (UTCTime, b, Text)
getIdShortIdAndTime = do
  now <- getCurrTime
  guid <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return (now, guid, shortId)

validateContext :: Text -> Context -> Flow ()
validateContext action context = do
  validateDomain Domain.MOBILITY context
  validateContextCommons action context
