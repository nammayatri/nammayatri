{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.BecknProvider.BP where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.App as TA
import Beckn.Types.Common
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.API.Update
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain as Domain
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
import qualified Product.Location as Location
import Storage.Queries.Location as Loc
import Storage.Queries.Organization as Org
import Storage.Queries.Person as Person
import Storage.Queries.ProductInstance as ProductInstance
import Storage.Queries.Vehicle as Vehicle
import qualified Test.RandomStrings as RS
import Utils.Common
import qualified Utils.Notifications as Notify

-- 1) Create Parent Case with Customer Request Details
-- 2) Notify all transporter using FCM
-- 3) Respond with Ack

search :: OrganizationId -> Organization -> SearchReq -> FlowHandler AckResponse
search transporterId bapOrg req = withFlowHandler $ do
  validateContext "search" $ req ^. #context
  uuid <- L.generateGUID
  transporter <- findOrganizationById transporterId
  when (transporter ^. #_enabled) $ do
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
    let bapOrgId = bapOrg ^. #_id
    deadDistance <- calculateDeadDistance transporter fromLocation
    let c = mkCase req uuid currTime validity startTime fromLocation toLocation transporterId bapOrgId deadDistance
    Case.create c
    admins <-
      Person.findAllByOrgIds
        [Person.ADMIN]
        [_getOrganizationId transporterId]
    Notify.notifyTransportersOnSearch c intent admins
  mkAckResponse uuid "search"
  where
    calculateDeadDistance organization fromLocation = do
      orgLocId <- LocationId <$> organization ^. #_locationId & fromMaybeM500 "ORG_HAS_NO_LOCATION"
      mbOrgLocation <- Loc.findLocationById orgLocId
      case mbOrgLocation of
        Nothing -> throwError500 "ORG_HAS_NO_LOCATION"
        Just orgLocation -> Location.calculateDistance orgLocation fromLocation

cancel :: OrganizationId -> Organization -> CancelReq -> FlowHandler AckResponse
cancel transporterId bapOrg req = withFlowHandler $ do
  validateContext "cancel" $ req ^. #context
  uuid <- L.generateGUID
  let prodInstId = req ^. #message . #order . #id -- transporter search productInstId
  prodInst <- ProductInstance.findById (ProductInstanceId prodInstId)
  piList <- ProductInstance.findAllByParentId (Just $ prodInst ^. #_id)
  Case.updateStatusByIds (ProductInstance._caseId <$> piList) SC.CLOSED
  case piList of
    [] -> return ()
    _ -> do
      trackerPi <- ProductInstance.findByIdType (ProductInstance._id <$> piList) SC.LOCATIONTRACKER
      orderPi <- ProductInstance.findByIdType (ProductInstance._id <$> piList) SC.RIDEORDER
      _ <- ProductInstance.updateStatus (ProductInstance._id trackerPi) ProductInstance.COMPLETED
      _ <- ProductInstance.updateStatus (ProductInstance._id orderPi) ProductInstance.CANCELLED
      return ()
  _ <- ProductInstance.updateStatus (ProductInstanceId prodInstId) ProductInstance.CANCELLED
  transporter <- findOrganizationById transporterId
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
  let bppShortId = _getShortOrganizationId $ transporter ^. #_shortId
  notifyCancelToGateway prodInstId callbackUrl bppShortId
  admins <-
    if transporter ^. #_enabled
      then Person.findAllByOrgIds [Person.ADMIN] [_getOrganizationId transporterId]
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

mkCase :: SearchReq -> Text -> UTCTime -> UTCTime -> UTCTime -> SL.Location -> SL.Location -> OrganizationId -> OrganizationId -> Maybe Float -> SC.Case
mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance = do
  let intent = req ^. #message . #intent
  let distance = Tag._value <$> find (\x -> x ^. #_key == "distance") (fromMaybe [] $ intent ^. #_tags)
  let tId = _getOrganizationId transporterId
  let bapId = _getOrganizationId bapOrgId
  SC.Case
    { _id = CaseId {_getCaseId = uuid},
      _name = Nothing,
      _description = Just "Case to search for a Ride",
      _shortId = tId <> "_" <> req ^. #context . #_transaction_id,
      _industry = SC.MOBILITY,
      _type = RIDESEARCH,
      _exchangeType = FULFILLMENT,
      _status = NEW,
      _startTime = startTime,
      _endTime = Nothing,
      _validTill = validity,
      _provider = Just tId,
      _providerType = Nothing,
      _requestor = Nothing,
      _requestorType = Just CONSUMER,
      _parentCaseId = Nothing,
      _fromLocationId = TA._getLocationId $ fromLocation ^. #_id,
      _toLocationId = TA._getLocationId $ toLocation ^. #_id,
      _udf1 = Just $ intent ^. #_vehicle . #variant,
      _udf2 = Just $ show $ length $ intent ^. #_payload . #_travellers,
      _udf3 = encodeToText <$> deadDistance,
      _udf4 = Just bapId,
      _udf5 = distance,
      _info = Nothing, --Just $ show $ req ^. #message
      _createdAt = now,
      _updatedAt = now
    }

confirm :: OrganizationId -> Organization -> ConfirmReq -> FlowHandler AckResponse
confirm transporterId bapOrg req = withFlowHandler $ do
  L.logInfo @Text "confirm API Flow" "Reached"
  validateContext "confirm" $ req ^. #context
  let prodInstId = ProductInstanceId $ req ^. #message . #order . #_id
  productInstance <- ProductInstance.findById prodInstId
  let transporterId' = OrganizationId $ productInstance ^. #_organizationId
  transporterOrg <- Org.findOrganizationById transporterId'
  unless (transporterId' == transporterId) (throwError400 "DIFFERENT_TRANSPORTER_IDS")
  let caseShortId = _getOrganizationId transporterId <> "_" <> req ^. #context . #_transaction_id
  searchCase <- Case.findBySid caseShortId
  bapOrgId <- searchCase ^. #_udf4 & fromMaybeM500 "BAP_ORG_NOT_SET"
  unless (bapOrg ^. #_id == OrganizationId bapOrgId) (throwError400 "BAP mismatch")

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

  -- Send callback to BAP
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
  let bppShortId = _getShortOrganizationId $ transporterOrg ^. #_shortId
  notifyGateway searchCase orderProductInstance trackerCase callbackUrl bppShortId

  admins <-
    if transporterOrg ^. #_enabled
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
  inAppOtpCode <- generateOTPCode
  return $
    ProductInstance.ProductInstance
      { _id = ProductInstanceId pid,
        _caseId = caseId,
        _productId = prodInst ^. #_productId,
        _personId = Nothing,
        _personUpdatedAt = Nothing,
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
        _udf4 = Just inAppOtpCode,
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
        _personUpdatedAt = Nothing,
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

notifyGateway :: Case -> ProductInstance -> Case -> BaseUrl -> Text -> Flow ()
notifyGateway c orderPi trackerCase callbackUrl bppShortId = do
  L.logInfo @Text "notifyGateway" $ show c
  allPis <- ProductInstance.findAllByCaseId (c ^. #_id)
  onConfirmPayload <- mkOnConfirmPayload c [orderPi] allPis trackerCase
  L.logInfo @Text "notifyGateway onConfirm Request Payload" $ show onConfirmPayload
  _ <- Gateway.onConfirm callbackUrl onConfirmPayload bppShortId
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
        _core_version = Just "0.8.2",
        _domain_version = Just "0.8.2",
        _transaction_id = tId,
        _message_id = tId,
        _bap_uri = Nothing,
        _bpp_uri = Nothing,
        _timestamp = currTime,
        _ttl = Nothing
      }

mkOnConfirmPayload :: Case -> [ProductInstance] -> [ProductInstance] -> Case -> Flow OnConfirmReq
mkOnConfirmPayload c pis _allPis trackerCase = do
  context <- mkContext "on_confirm" $ c ^. #_shortId -- TODO : What should be the txnId
  trip <- mkTrip trackerCase (head pis)
  order <- GT.mkOrder c (head pis) (Just trip)
  return
    CallbackReq
      { context,
        contents = Right $ ConfirmOrder order
      }

serviceStatus :: OrganizationId -> Organization -> StatusReq -> FlowHandler StatusRes
serviceStatus transporterId bapOrg req = withFlowHandler $ do
  L.logInfo @Text "serviceStatus API Flow" $ show req
  let piId = req ^. #message . #order . #id -- transporter search product instance id
  trackerPi <- ProductInstance.findByParentIdType (Just $ ProductInstanceId piId) SC.LOCATIONTRACKER
  --TODO : use forkFlow to notify gateway
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
  transporter <- findOrganizationById transporterId
  let bppShortId = _getShortOrganizationId $ transporter ^. #_shortId
  notifyServiceStatusToGateway piId trackerPi callbackUrl bppShortId
  uuid <- L.generateGUID
  mkAckResponse uuid "status"

notifyServiceStatusToGateway :: Text -> ProductInstance -> BaseUrl -> Text -> Flow ()
notifyServiceStatusToGateway piId trackerPi callbackUrl bppShortId = do
  onServiceStatusPayload <- mkOnServiceStatusPayload piId trackerPi
  L.logInfo @Text "notifyServiceStatusToGateway Request" $ show onServiceStatusPayload
  _ <- Gateway.onStatus callbackUrl onServiceStatusPayload bppShortId
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

trackTrip :: OrganizationId -> Organization -> TrackTripReq -> FlowHandler TrackTripRes
trackTrip transporterId org req = withFlowHandler $ do
  L.logInfo @Text "track trip API Flow" $ show req
  validateContext "track" $ req ^. #context
  let tripId = req ^. #message . #order_id
  case_ <- Case.findById (CaseId tripId)
  case case_ ^. #_parentCaseId of
    Just parentCaseId -> do
      parentCase <- Case.findById parentCaseId
      --TODO : use forkFlow to notify gateway
      callbackUrl <- org ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
      transporter <- findOrganizationById transporterId
      let bppShortId = _getShortOrganizationId $ transporter ^. #_shortId
      notifyTripUrlToGateway case_ parentCase callbackUrl bppShortId
      uuid <- L.generateGUID
      mkAckResponse uuid "track"
    Nothing -> throwError400 "Case does not have an associated parent case"

notifyTripUrlToGateway :: Case -> Case -> BaseUrl -> Text -> Flow ()
notifyTripUrlToGateway case_ parentCase callbackUrl bppShortId = do
  onTrackTripPayload <- mkOnTrackTripPayload case_ parentCase
  L.logInfo @Text "notifyTripUrlToGateway Request" $ show onTrackTripPayload
  _ <- Gateway.onTrackTrip callbackUrl onTrackTripPayload bppShortId
  return ()

notifyTripInfoToGateway :: ProductInstance -> Case -> Case -> BaseUrl -> Text -> Flow ()
notifyTripInfoToGateway prodInst trackerCase parentCase callbackUrl bppShortId = do
  onUpdatePayload <- mkOnUpdatePayload prodInst trackerCase parentCase
  L.logInfo @Text "notifyTripInfoToGateway Request" $ show onUpdatePayload
  _ <- Gateway.onUpdate callbackUrl onUpdatePayload bppShortId
  return ()

notifyCancelToGateway :: Text -> BaseUrl -> Text -> Flow ()
notifyCancelToGateway prodInstId callbackUrl bppShortId = do
  onCancelPayload <- mkCancelRidePayload prodInstId -- search product instance id
  L.logInfo @Text "notifyGateway Request" $ show onCancelPayload
  _ <- Gateway.onCancel callbackUrl onCancelPayload bppShortId
  return ()

mkOnTrackTripPayload :: Case -> Case -> Flow OnTrackTripReq
mkOnTrackTripPayload trackerCase parentCase = do
  context <- mkContext "on_track" $ last $ T.split (== '_') $ parentCase ^. #_shortId
  let data_url = GT.baseTrackingUrl <> "/" <> _getCaseId (trackerCase ^. #_id)
  let tracking = GT.mkTracking "PULL" data_url
  return
    CallbackReq
      { context,
        contents = Right $ OnTrackReqMessage (Just tracking)
      }

mkTrip :: Case -> ProductInstance -> Flow Trip
mkTrip c orderPi = do
  prodInst <- ProductInstance.findByCaseId $ c ^. #_id
  driver <- mapM mkDriverInfo $ prodInst ^. #_personId
  vehicle <- join <$> mapM mkVehicleInfo (prodInst ^. #_entityId)
  tripCode <- orderPi ^. #_udf4 & fromMaybeM500 "IN_APP_OTP_MISSING"
  L.logInfo @Text "vehicle" $ show vehicle
  return $
    Trip
      { id = tripCode,
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
  trip <- mkTrip case_ prodInst
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
