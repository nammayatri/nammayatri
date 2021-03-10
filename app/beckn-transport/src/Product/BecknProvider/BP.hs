{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.BP where

import App.Types
  ( AppEnv (..),
    Flow,
    FlowHandler,
    Log (..),
  )
import Beckn.Product.Validation.Context
  ( validateContextCommons,
    validateDomain,
  )
import qualified Beckn.Types.App as App
import Beckn.Types.Common (GuidLike (..))
import qualified Beckn.Types.Core.API.Callback as API
import qualified Beckn.Types.Core.API.Cancel as API
import qualified Beckn.Types.Core.API.Status as API
import qualified Beckn.Types.Core.API.Track as API
import qualified Beckn.Types.Core.API.Update as API
import qualified Beckn.Types.Core.Ack as Ack
import Beckn.Types.Core.Context (Context (..))
import qualified Beckn.Types.Core.Domain as Domain
import Beckn.Types.Core.Order
  ( Order (..),
    OrderItem (OrderItem),
  )
import Beckn.Types.Mobility.Driver (Driver)
import Beckn.Types.Mobility.Payload (Payload (..))
import Beckn.Types.Mobility.Trip (Trip (..))
import qualified Beckn.Types.Mobility.Vehicle as BVehicle
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
  ( fromMaybeM500,
    getCurrTime,
    mkAckResponse,
    throwError400,
    withFlowHandler,
  )
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified External.Gateway.Transform as GT
import qualified Models.Case as Case
import Servant.Client (BaseUrl (baseUrlPath))
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.Vehicle as Vehicle
import qualified Test.RandomStrings as RS
import Types.App (DriverId (..), RideId (..), RideRequestId (..))
import qualified Types.Storage.RideRequest as SRideRequest
import qualified Utils.Notifications as Notify

cancel :: App.OrganizationId -> Organization.Organization -> API.CancelReq -> FlowHandler Ack.AckResponse
cancel _transporterId _bapOrg req = withFlowHandler $ do
  validateContext "cancel" $ req ^. #context
  let prodInstId = req ^. #message . #order . #id -- transporter search productInstId
  prodInst <- ProductInstance.findById (App.ProductInstanceId prodInstId)
  piList <- ProductInstance.findAllByParentId (Just $ prodInst ^. #_id)
  orderPi <- ProductInstance.findByIdType (ProductInstance._id <$> piList) Case.RIDEORDER
  RideRequest.create =<< mkRideReq (orderPi ^. #_id) SRideRequest.CANCELLATION
  uuid <- L.generateGUID
  mkAckResponse uuid "cancel"

cancelRide :: RideId -> Flow ()
cancelRide rideId = do
  orderPi <- ProductInstance.findById . App.ProductInstanceId $ _getRideId rideId
  searchPiId <- ProductInstance._parentId orderPi & fromMaybeM500 "RIDEORDER_DOESNT_HAVE_PARENT"
  piList <- ProductInstance.findAllByParentId (Just searchPiId)
  Case.updateStatusByIds (ProductInstance._caseId <$> piList) Case.CLOSED
  trackerPi <- ProductInstance.findByIdType (ProductInstance._id <$> piList) Case.LOCATIONTRACKER
  _ <- ProductInstance.updateStatus searchPiId ProductInstance.CANCELLED
  _ <- ProductInstance.updateStatus (ProductInstance._id trackerPi) ProductInstance.COMPLETED
  _ <- ProductInstance.updateStatus (ProductInstance._id orderPi) ProductInstance.CANCELLED

  orderCase <- Case.findById (orderPi ^. #_caseId)
  bapOrgId <- Case._udf4 orderCase & fromMaybeM500 "BAP_ORG_ID_NOT_PRESENT"
  bapOrg <- Organization.findOrganizationById $ App.OrganizationId bapOrgId
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
  let transporterId = App.OrganizationId $ ProductInstance._organizationId orderPi
  transporter <- Organization.findOrganizationById transporterId
  let bppShortId = App._getShortOrganizationId $ transporter ^. #_shortId
  notifyCancelToGateway (App._getProductInstanceId searchPiId) callbackUrl bppShortId

  case piList of
    [] -> pure ()
    prdInst : _ -> do
      c <- Case.findById $ prdInst ^. #_caseId
      case prdInst ^. #_personId of
        Nothing -> pure ()
        Just driverId -> do
          driver <- Person.findPersonById driverId
          DriverInformation.updateOnRideFlow (DriverId $ App._getPersonId driverId) False
          Notify.notifyDriverOnCancel c driver

-- TODO : Add notifying transporter admin with FCM

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

serviceStatus :: App.OrganizationId -> Organization.Organization -> API.StatusReq -> FlowHandler API.StatusRes
serviceStatus transporterId bapOrg req = withFlowHandler $ do
  logInfo "serviceStatus API Flow" $ show req
  let piId = req ^. #message . #order . #id -- transporter search product instance id
  trackerPi <- ProductInstance.findByParentIdType (Just $ App.ProductInstanceId piId) Case.LOCATIONTRACKER
  --TODO : use forkFlow to notify gateway
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
  transporter <- Organization.findOrganizationById transporterId
  let bppShortId = App._getShortOrganizationId $ transporter ^. #_shortId
  notifyServiceStatusToGateway piId trackerPi callbackUrl bppShortId
  uuid <- L.generateGUID
  mkAckResponse uuid "status"

notifyServiceStatusToGateway :: Text -> ProductInstance.ProductInstance -> App.BaseUrl -> Text -> Flow ()
notifyServiceStatusToGateway piId trackerPi callbackUrl bppShortId = do
  onServiceStatusPayload <- mkOnServiceStatusPayload piId trackerPi
  logInfo "notifyServiceStatusToGateway Request" $ show onServiceStatusPayload
  _ <- Gateway.onStatus callbackUrl onServiceStatusPayload bppShortId
  return ()

mkOnServiceStatusPayload :: Text -> ProductInstance.ProductInstance -> Flow API.OnStatusReq
mkOnServiceStatusPayload piId trackerPi = do
  context <- mkContext "on_status" "" -- FIXME: transaction id?
  order <- mkOrderRes piId (App._getProductsId $ trackerPi ^. #_productId) (show $ trackerPi ^. #_status)
  let onStatusMessage = API.OnStatusReqMessage order
  return $
    API.CallbackReq
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

trackTrip :: App.OrganizationId -> Organization.Organization -> API.TrackTripReq -> FlowHandler API.TrackTripRes
trackTrip transporterId org req = withFlowHandler $ do
  logInfo "track trip API Flow" $ show req
  validateContext "track" $ req ^. #context
  let tripId = req ^. #message . #order_id
  case_ <- Case.findById (App.CaseId tripId)
  case case_ ^. #_parentCaseId of
    Just parentCaseId -> do
      parentCase <- Case.findById parentCaseId
      --TODO : use forkFlow to notify gateway
      callbackUrl <- org ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
      transporter <- Organization.findOrganizationById transporterId
      let bppShortId = App._getShortOrganizationId $ transporter ^. #_shortId
      notifyTripUrlToGateway case_ parentCase callbackUrl bppShortId
      uuid <- L.generateGUID
      mkAckResponse uuid "track"
    Nothing -> throwError400 "Case does not have an associated parent case"

notifyTripUrlToGateway :: Case.Case -> Case.Case -> App.BaseUrl -> Text -> Flow ()
notifyTripUrlToGateway case_ parentCase callbackUrl bppShortId = do
  onTrackTripPayload <- mkOnTrackTripPayload case_ parentCase
  logInfo "notifyTripUrlToGateway Request" $ show onTrackTripPayload
  _ <- Gateway.onTrackTrip callbackUrl onTrackTripPayload bppShortId
  return ()

notifyTripInfoToGateway :: ProductInstance.ProductInstance -> Case.Case -> Case.Case -> App.BaseUrl -> Text -> Flow ()
notifyTripInfoToGateway prodInst trackerCase parentCase callbackUrl bppShortId = do
  onUpdatePayload <- mkOnUpdatePayload prodInst trackerCase parentCase
  logInfo "notifyTripInfoToGateway Request" $ show onUpdatePayload
  _ <- Gateway.onUpdate callbackUrl onUpdatePayload bppShortId
  return ()

notifyCancelToGateway :: Text -> App.BaseUrl -> Text -> Flow ()
notifyCancelToGateway prodInstId callbackUrl bppShortId = do
  onCancelPayload <- mkCancelRidePayload prodInstId -- search product instance id
  logInfo "notifyGateway Request" $ show onCancelPayload
  _ <- Gateway.onCancel callbackUrl onCancelPayload bppShortId
  return ()

mkOnTrackTripPayload :: Case.Case -> Case.Case -> Flow API.OnTrackTripReq
mkOnTrackTripPayload trackerCase parentCase = do
  context <- mkContext "on_track" $ last $ T.split (== '_') $ parentCase ^. #_shortId
  let data_url = GT.baseTrackingUrl <> "/" <> App._getCaseId (trackerCase ^. #_id)
  let tracking = GT.mkTracking "PULL" data_url
  return
    API.CallbackReq
      { context,
        contents = Right $ API.OnTrackReqMessage (Just tracking)
      }

mkTrip :: Case.Case -> ProductInstance.ProductInstance -> Flow Trip
mkTrip c orderPi = do
  prodInst <- ProductInstance.findByCaseId $ c ^. #_id
  driver <- mapM mkDriverInfo $ prodInst ^. #_personId
  vehicle <- join <$> mapM mkVehicleInfo (prodInst ^. #_entityId)
  tripCode <- orderPi ^. #_udf4 & fromMaybeM500 "IN_APP_OTP_MISSING"
  logInfo "vehicle" $ show vehicle
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

mkOnUpdatePayload :: ProductInstance.ProductInstance -> Case.Case -> Case.Case -> Flow API.OnUpdateReq
mkOnUpdatePayload prodInst case_ pCase = do
  context <- mkContext "on_update" $ App._getProductInstanceId $ prodInst ^. #_id
  trip <- mkTrip case_ prodInst
  order <- GT.mkOrder pCase prodInst (Just trip)
  return
    API.CallbackReq
      { context,
        contents = Right $ API.OnUpdateOrder order
      }

mkDriverInfo :: App.PersonId -> Flow Driver
mkDriverInfo driverId = do
  person <- Person.findPersonById driverId
  return $ GT.mkDriverObj person

mkVehicleInfo :: Text -> Flow (Maybe BVehicle.Vehicle)
mkVehicleInfo vehicleId = do
  vehicle <- Vehicle.findVehicleById (App.VehicleId vehicleId)
  return $ GT.mkVehicleObj <$> vehicle

mkCancelRidePayload :: Text -> Flow API.OnCancelReq
mkCancelRidePayload prodInstId = do
  context <- mkContext "on_cancel" "" -- FIXME: transaction id?
  tripObj <- mkCancelTripObj prodInstId
  return
    API.CallbackReq
      { context,
        contents = Right tripObj
      }

mkCancelTripObj :: Text -> Flow Trip
mkCancelTripObj prodInstId = do
  productInstance <- ProductInstance.findById (App.ProductInstanceId prodInstId)
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

mkRideReq :: App.ProductInstanceId -> SRideRequest.RideRequestType -> Flow SRideRequest.RideRequest
mkRideReq pId rideRequestType = do
  guid <- generateGUID
  currTime <- getCurrTime
  let rideId = RideId $ App._getProductInstanceId pId
  pure
    SRideRequest.RideRequest
      { _id = RideRequestId guid,
        _rideId = rideId,
        _createdAt = currTime,
        _type = rideRequestType
      }

makeBppUrl :: Organization.Organization -> App.BaseUrl -> App.BaseUrl
makeBppUrl transporterOrg url =
  let orgId = App._getOrganizationId $ transporterOrg ^. #_id
      newPath = baseUrlPath url <> "/" <> T.unpack orgId
   in url {baseUrlPath = newPath}
