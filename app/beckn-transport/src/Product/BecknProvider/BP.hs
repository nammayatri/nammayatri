{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.BP where

import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Location as BL
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Service
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Storage.Case as SC
import Beckn.Types.Storage.CaseProduct as CaseProduct
import Beckn.Types.Storage.Location as SL
import Beckn.Types.Storage.Organization as Org
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Accessor as Lens
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import External.Gateway.Flow as Gateway
import External.Gateway.Transform as GT
import Servant
import Storage.Queries.Case as Case
import Storage.Queries.CaseProduct as CaseProduct
import Storage.Queries.Location as Loc
import Storage.Queries.Organization as Org
import Storage.Queries.Person as Person
import Storage.Queries.Products as Product
import System.Environment
import qualified Test.RandomStrings as RS
import Types.Notification
import Utils.FCM

-- 1) Create Parent Case with Customer Request Details
-- 2) Notify all transporter using GCM
-- 3) Respond with Ack

search :: SearchReq -> FlowHandler AckResponse
search req = withFlowHandler $ do
  --TODO: Need to add authenticator
  uuid <- L.generateGUID
  currTime <- getCurrentTimeUTC
  validity <- getValidTime $ req ^. #message ^. #time
  uuid1 <- L.generateGUID
  let fromLocation = mkFromLocation req uuid1 currTime $ req ^. #message ^. #origin
  uuid2 <- L.generateGUID
  let toLocation = mkFromLocation req uuid2 currTime $ req ^. #message ^. #destination
  Loc.create fromLocation
  Loc.create toLocation
  let c = mkCase req uuid currTime validity fromLocation toLocation
  Case.create c
  transporters <- listOrganizations Nothing Nothing [Org.TRANSPORTER] [Org.APPROVED]
  -- TODO : Fix show
  admins <-
    findAllByOrgIds
      [Person.ADMIN]
      ((\o -> _getOrganizationId $ Org._id o) <$> transporters)
  -- notifyTransporters c admins --TODO : Uncomment this once we start saving deviceToken
  mkAckResponse uuid "search"

notifyTransporters :: Case -> [Person] -> L.Flow ()
notifyTransporters c admins =
  -- TODO : Get Token from Person
  traverse_ (\p -> sendNotification mkCaseNotification "deviceToken") admins
  where
    mkCaseNotification =
      Notification
        { _type = LEAD,
          _payload = c
        }

getValidTime :: LocalTime -> L.Flow LocalTime
getValidTime now = pure $ addLocalTime (60 * 30 :: NominalDiffTime) now

mkFromLocation :: SearchReq -> Text -> LocalTime -> BL.Location -> SL.Location
mkFromLocation req uuid now loc = do
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
  let intent = (req ^. #message)
  SC.Case
    { _id = CaseId {_getCaseId = uuid},
      _name = Nothing,
      _description = Just "Case to create a Ride",
      _shortId = req ^. #context ^. #transaction_id,
      _industry = SC.MOBILITY,
      _type = RIDEBOOK,
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
      _udf1 = Just $ intent ^. #vehicle ^. #variant,
      _udf2 = Just $ show $ intent ^. #payload ^. #travellers ^. #count,
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
  let prodId = (req ^. #message ^. #_selected_items) !! 0
  let caseShortId = req ^. #context ^. #transaction_id -- change to message.transactionId
  case_ <- Case.findBySid caseShortId
  let caseId = _getCaseId $ case_ ^. #_id
  Case.updateStatus (CaseId caseId) SC.CONFIRMED
  CaseProduct.updateStatus (CaseId caseId) (ProductsId prodId) CaseProduct.CONFIRMED
  Product.updateStatus (ProductsId prodId) Product.CONFIRMED
  --TODO: need to update other product status to VOID for this case
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  uuid <- L.generateGUID
  currTime <- getCurrentTimeUTC
  let trackerCase = mkTrackerCase case_ uuid currTime $ T.pack shortId
  Case.create trackerCase
  uuid1 <- L.generateGUID
  let trackerCaseProduct = mkTrackerCaseProduct uuid1 (trackerCase ^. #_id) (ProductsId prodId) currTime
  CaseProduct.create trackerCaseProduct
  notifyGateway case_ prodId trackerCase
  mkAckResponse uuid "confirm"

-- TODO : Add notifying transporter admin with GCM

mkTrackerCaseProduct :: Text -> CaseId -> ProductsId -> LocalTime -> CaseProduct.CaseProduct
mkTrackerCaseProduct cpId caseId prodId currTime =
  CaseProduct.CaseProduct
    { _id = CaseProductId cpId,
      _caseId = caseId,
      _productId = prodId,
      _personId = Nothing,
      _quantity = 1,
      _price = 0,
      _status = CaseProduct.INPROGRESS,
      _info = Nothing,
      _createdAt = currTime,
      _updatedAt = currTime
    }

mkTrackerCase :: SC.Case -> Text -> LocalTime -> Text -> SC.Case
mkTrackerCase case_ uuid now shortId = do
  SC.Case
    { _id = CaseId {_getCaseId = uuid},
      _name = Nothing,
      _description = Just "Case to track a Ride",
      _shortId = shortId,
      _industry = SC.MOBILITY,
      _type = TRACKER,
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

notifyGateway :: Case -> Text -> Case -> L.Flow ()
notifyGateway c prodId trackerCase = do
  L.logInfo "notifyGateway" $ show c
  cps <- CaseProduct.findAllByCaseId (c ^. #_id)
  L.logInfo "notifyGateway" $ show cps
  prods <- Product.findAllById $ (\cp -> (cp ^. #_productId)) <$> cps
  onConfirmPayload <- mkOnConfirmPayload c prods trackerCase
  L.logInfo "notifyGateway onConfirm Request Payload" $ show onConfirmPayload
  Gateway.onConfirm onConfirmPayload
  return ()

mkOnConfirmPayload :: Case -> [Products] -> Case -> L.Flow OnConfirmReq
mkOnConfirmPayload c prods trackerCase = do
  currTime <- getCurrTime
  let context =
        Context
          { domain = "MOBILITY",
            action = "CONFIRM",
            version = Just $ "0.1",
            transaction_id = c ^. #_shortId, -- TODO : What should be the txnId
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  trip <- GT.mkTrip $ Just trackerCase
  service <- GT.mkServiceOffer c prods trip
  return
    OnConfirmReq
      { context,
        message = service
      }

serviceStatus :: StatusReq -> FlowHandler StatusRes
serviceStatus req = withFlowHandler $ do
  L.logInfo "serviceStatus API Flow" $ show req
  let caseId = req ^. #message ^. #id
  -- c <- Case.findByIdAndType (CaseId caseId) "RIDEBOOK"
  --TODO: Need to fetch child case and Trip.Tracking Record
  --TODO: Need to fetch confirmed product and add Trip.Vehicle and Trip.Driver Record
  c <- Case.findById (CaseId caseId)
  cps <- CaseProduct.findAllByCaseId (c ^. #_id)
  prods <- Product.findAllById $ (\cp -> (cp ^. #_productId)) <$> cps
  --TODO : use forkFlow to notify gateway
  trackerCase <- Case.findByParentCaseIdAndType (CaseId caseId) SC.TRACKER
  notifyServiceStatusToGateway c prods trackerCase
  uuid <- L.generateGUID
  mkAckResponse uuid "track"

notifyServiceStatusToGateway :: Case -> [Products] -> Maybe Case -> L.Flow ()
notifyServiceStatusToGateway c prods trackerCase = do
  onServiceStatusPayload <- mkOnServiceStatusPayload c prods trackerCase
  L.logInfo "notifyServiceStatusToGateway Request" $ show onServiceStatusPayload
  Gateway.onStatus onServiceStatusPayload
  return ()

mkOnServiceStatusPayload :: Case -> [Products] -> Maybe Case -> L.Flow OnStatusReq
mkOnServiceStatusPayload c prods trackerCase = do
  currTime <- getCurrTime
  let context =
        Context
          { domain = "MOBILITY",
            action = "on_status",
            version = Just $ "0.1",
            transaction_id = c ^. #_shortId,
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  trip <- GT.mkTrip trackerCase
  service <- GT.mkServiceOffer c prods trip
  return
    OnStatusReq
      { context,
        message = service
      }

trackTrip :: TrackTripReq -> FlowHandler TrackTripRes
trackTrip req = withFlowHandler $ do
  L.logInfo "track trip API Flow" $ show req
  let tripId = req ^. #message ^. #id
  case_ <- Case.findById (CaseId tripId)
  --TODO : use forkFlow to notify gateway
  notifyTripUrlToGateway case_
  uuid <- L.generateGUID
  mkAckResponse uuid "track"

notifyTripUrlToGateway :: Case -> L.Flow ()
notifyTripUrlToGateway c = do
  onTrackTripPayload <- mkOnTrackTripPayload $ c ^. #_shortId
  url <- L.runIO $ getEnv "BECKN_GATEWAY_BASE_URL"
  L.logInfo "notifyTripUrlToGateway Request" $ show onTrackTripPayload
  Gateway.onTrackTrip onTrackTripPayload
  return ()

mkOnTrackTripPayload :: Text -> L.Flow OnTrackTripReq
mkOnTrackTripPayload c = do
  currTime <- getCurrTime
  let context =
        Context
          { domain = "MOBILITY",
            action = "on_track",
            version = Just $ "0.1",
            transaction_id = c,
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  let data_url = GT.baseTrackingUrl <> "/" <> c
  let embed_url = GT.baseTrackingUrl <> "/" <> c <> "/embed"
  return
    OnTrackTripReq
      { context,
        message = GT.mkTracking "PULL" data_url embed_url
      }
