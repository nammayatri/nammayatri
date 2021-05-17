{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.BP where

import App.Types (AppEnv (..), Flow, FlowHandler)
import Beckn.Product.Validation.Context
  ( validateContextCommons,
    validateDomain,
  )
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Callback as API
import qualified Beckn.Types.Core.API.Cancel as API
import qualified Beckn.Types.Core.API.Status as API
import qualified Beckn.Types.Core.API.Track as API
import qualified Beckn.Types.Core.API.Update as API
import Beckn.Types.Core.Ack (AckResponse (..), Status (..), ack)
import qualified Beckn.Types.Core.Ack as Ack
import Beckn.Types.Core.Context (Context (..))
import qualified Beckn.Types.Core.Domain as Domain
import Beckn.Types.Core.Order
  ( Order (..),
    OrderItem (OrderItem),
  )
import Beckn.Types.Id
import Beckn.Types.Mobility.Driver (Driver)
import Beckn.Types.Mobility.Payload (Payload (..))
import Beckn.Types.Mobility.Trip (Trip (..))
import qualified Beckn.Types.Mobility.Vehicle as BVehicle
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified External.Gateway.Transform as GT
import qualified Models.Case as Case
import Servant.Client (BaseUrl (baseUrlPath))
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.Vehicle as Vehicle
import qualified Test.RandomStrings as RS
import Types.App (Ride)
import Types.Error
import qualified Types.Storage.RideRequest as SRideRequest
import Utils.Common
import qualified Utils.Notifications as Notify

cancel :: Id Organization.Organization -> Organization.Organization -> API.CancelReq -> FlowHandler Ack.AckResponse
cancel _transporterId _bapOrg req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    let context = req ^. #context
    validateContext "cancel" context
    let prodInstId = req ^. #message . #order . #id -- transporter search productInstId
    prodInst <- ProductInstance.findById (Id prodInstId)
    piList <- ProductInstance.findAllByParentId (prodInst ^. #_id)
    orderPi <- ProductInstance.findByIdType (ProductInstance._id <$> piList) Case.RIDEORDER
    RideRequest.createFlow =<< mkRideReq (orderPi ^. #_id) SRideRequest.CANCELLATION
    return $ AckResponse context (ack ACK) Nothing

cancelRide :: Id Ride -> Bool -> Flow ()
cancelRide rideId requestedByDriver = do
  orderPi <- ProductInstance.findById $ cast rideId
  searchPiId <- ProductInstance._parentId orderPi & fromMaybeM (PIFieldNotPresent "parent_id")
  piList <- ProductInstance.findAllByParentId searchPiId
  trackerPi <- ProductInstance.findByIdType (ProductInstance._id <$> piList) Case.LOCATIONTRACKER
  cancelRideTransaction piList searchPiId (trackerPi ^. #_id) (orderPi ^. #_id) requestedByDriver

  orderCase <- Case.findById (orderPi ^. #_caseId)
  bapOrgId <- Case._udf4 orderCase & fromMaybeM (CaseFieldNotPresent "udf4")
  bapOrg <- Organization.findOrganizationById $ Id bapOrgId
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let transporterId = Id $ ProductInstance._organizationId orderPi
  transporter <- Organization.findOrganizationById transporterId
  let bppShortId = getShortId $ transporter ^. #_shortId
  searchCaseId <- orderCase ^. #_parentCaseId & fromMaybeM (CaseFieldNotPresent "parentCaseId")
  searchCase <- Case.findById searchCaseId
  let txnId = last . T.splitOn "_" $ searchCase ^. #_shortId
  bapUri <- searchCase ^. #_udf4 & fromMaybeM (CaseFieldNotPresent "udf4") >>= parseBaseUrl
  notifyCancelToGateway searchPiId callbackUrl bppShortId txnId bapUri

  case piList of
    [] -> pure ()
    prdInst : _ -> do
      c <- Case.findById $ prdInst ^. #_caseId
      case prdInst ^. #_personId of
        Nothing -> pure ()
        Just driverId -> do
          driver <- Person.findPersonById driverId
          Notify.notifyDriverOnCancel c driver

cancelRideTransaction ::
  [ProductInstance.ProductInstance] ->
  Id ProductInstance.ProductInstance ->
  Id ProductInstance.ProductInstance ->
  Id ProductInstance.ProductInstance ->
  Bool ->
  Flow ()
cancelRideTransaction piList searchPiId trackerPiId orderPiId requestedByDriver = DB.runSqlDBTransaction $ do
  case piList of
    [] -> pure ()
    (prdInst : _) -> do
      QCase.updateStatusByIds (ProductInstance._caseId <$> piList) Case.CLOSED
      let mbPersonId = prdInst ^. #_personId
      whenJust mbPersonId updateDriverInfo
  ProductInstance.updateStatus searchPiId ProductInstance.CANCELLED
  ProductInstance.updateStatus trackerPiId ProductInstance.COMPLETED
  ProductInstance.updateStatus orderPiId ProductInstance.CANCELLED
  where
    updateDriverInfo personId = do
      let driverId = cast personId
      DriverInformation.updateOnRide driverId False
      when requestedByDriver $ QDriverStats.updateIdleTime driverId

serviceStatus :: Id Organization.Organization -> Organization.Organization -> API.StatusReq -> FlowHandler API.StatusRes
serviceStatus transporterId bapOrg req = withFlowHandlerAPI $ do
  logTagInfo "serviceStatus API Flow" $ show req
  let context = req ^. #context
  let piId = Id $ req ^. #message . #order . #id -- transporter search product instance id
  trackerPi <- ProductInstance.findByParentIdType piId Case.LOCATIONTRACKER
  --TODO : use forkFlow to notify gateway
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  transporter <- Organization.findOrganizationById transporterId
  let bppShortId = getShortId $ transporter ^. #_shortId
  notifyServiceStatusToGateway piId trackerPi callbackUrl bppShortId $ context ^. #_transaction_id
  return $ AckResponse context (ack ACK) Nothing

notifyServiceStatusToGateway :: Id ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> BaseUrl -> Text -> Text -> Flow ()
notifyServiceStatusToGateway piId trackerPi callbackUrl bppShortId txnId = do
  onServiceStatusPayload <- mkOnServiceStatusPayload piId trackerPi txnId
  logTagInfo "notifyServiceStatusToGateway Request" $ show onServiceStatusPayload
  _ <- Gateway.onStatus callbackUrl onServiceStatusPayload bppShortId
  return ()

mkOnServiceStatusPayload :: Id ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> Text -> Flow API.OnStatusReq
mkOnServiceStatusPayload piId trackerPi txnId = do
  bapUri <- fetchBapUri
  bppUri <- asks xAppUri
  context <- buildContext "on_status" txnId (Just bapUri) (Just bppUri)
  order <- mkOrderRes piId (getId $ trackerPi ^. #_productId) (show $ trackerPi ^. #_status)
  let onStatusMessage = API.OnStatusReqMessage order
  return $
    API.CallbackReq
      { context = context,
        contents = Right onStatusMessage
      }
  where
    fetchBapUri = do
      searchPi <- ProductInstance.findById piId
      searchCase <- Case.findById (searchPi ^. #_caseId)
      (searchCase ^. #_udf4 & fromMaybeM (CaseFieldNotPresent "udf4"))
        >>= parseBaseUrl
    mkOrderRes prodInstId productId status = do
      now <- getCurrentTime
      return $
        Order
          { _id = getId prodInstId,
            _state = T.pack status,
            _items = [OrderItem productId Nothing],
            _created_at = now,
            _updated_at = now,
            _billing = Nothing,
            _payment = Nothing,
            _update_action = Nothing,
            _quotation = Nothing
          }

trackTrip :: Id Organization.Organization -> Organization.Organization -> API.TrackTripReq -> FlowHandler API.TrackTripRes
trackTrip transporterId org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "track trip API Flow" $ show req
    let context = req ^. #context
    validateContext "track" context
    let tripId = req ^. #message . #order_id
    case_ <- Case.findById $ Id tripId
    --TODO : use forkFlow to notify gateway
    callbackUrl <- org ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    transporter <- Organization.findOrganizationById transporterId
    let bppShortId = getShortId $ transporter ^. #_shortId
    onTrackContext <- updateContext "on_track" context
    notifyTripUrlToGateway case_ onTrackContext callbackUrl bppShortId
    return $ AckResponse context (ack ACK) Nothing

notifyTripUrlToGateway :: Case.Case -> Context -> BaseUrl -> Text -> Flow ()
notifyTripUrlToGateway case_ context callbackUrl bppShortId = do
  onTrackTripPayload <- mkOnTrackTripPayload case_ context
  logTagInfo "notifyTripUrlToGateway Request" $ show onTrackTripPayload
  _ <- Gateway.onTrackTrip callbackUrl onTrackTripPayload bppShortId
  return ()

notifyTripInfoToGateway :: ProductInstance.ProductInstance -> Case.Case -> Case.Case -> BaseUrl -> Text -> Flow ()
notifyTripInfoToGateway prodInst trackerCase parentCase callbackUrl bppShortId = do
  onUpdatePayload <- mkOnUpdatePayload prodInst trackerCase parentCase
  logTagInfo "notifyTripInfoToGateway Request" $ show onUpdatePayload
  _ <- Gateway.onUpdate callbackUrl onUpdatePayload bppShortId
  return ()

notifyCancelToGateway :: Id ProductInstance.ProductInstance -> BaseUrl -> Text -> Text -> BaseUrl -> Flow ()
notifyCancelToGateway prodInstId callbackUrl bppShortId txnId bapUri = do
  onCancelPayload <- mkCancelRidePayload prodInstId txnId bapUri -- search product instance id
  logTagInfo "notifyGateway Request" $ show onCancelPayload
  _ <- Gateway.onCancel callbackUrl onCancelPayload bppShortId
  return ()

mkOnTrackTripPayload :: Case.Case -> Context -> Flow API.OnTrackTripReq
mkOnTrackTripPayload trackerCase context = do
  let data_url = GT.baseTrackingUrl <> "/" <> getId (trackerCase ^. #_id)
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
  tripCode <- orderPi ^. #_udf4 & fromMaybeM (PIFieldNotPresent "udf4")
  logTagInfo "vehicle" $ show vehicle
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
  let txnId = last . T.splitOn "_" $ pCase ^. #_shortId
  bapUri <- pCase ^. #_udf4 & fromMaybeM (CaseFieldNotPresent "udf4") >>= parseBaseUrl
  bppUri <- asks xAppUri
  context <- buildContext "on_update" txnId (Just bapUri) (Just bppUri)
  trip <- mkTrip case_ prodInst
  order <- GT.mkOrder prodInst (Just trip)
  return
    API.CallbackReq
      { context,
        contents = Right $ API.OnUpdateOrder order
      }

mkDriverInfo :: Id Person.Person -> Flow Driver
mkDriverInfo driverId = do
  person <- Person.findPersonById driverId
  return $ GT.mkDriverObj person

mkVehicleInfo :: Text -> Flow (Maybe BVehicle.Vehicle)
mkVehicleInfo vehicleId = do
  vehicle <- Vehicle.findVehicleById (Id vehicleId)
  return $ GT.mkVehicleObj <$> vehicle

mkCancelRidePayload :: Id ProductInstance.ProductInstance -> Text -> BaseUrl -> Flow API.OnCancelReq
mkCancelRidePayload prodInstId txnId bapUri = do
  bppUri <- asks xAppUri
  context <- buildContext "on_cancel" txnId (Just bapUri) (Just bppUri)
  tripObj <- mkCancelTripObj prodInstId
  return
    API.CallbackReq
      { context,
        contents = Right tripObj
      }

mkCancelTripObj :: Id ProductInstance.ProductInstance -> Flow Trip
mkCancelTripObj prodInstId = do
  productInstance <- ProductInstance.findById prodInstId
  driver <- mapM mkDriverInfo $ productInstance ^. #_personId
  vehicle <- join <$> mapM mkVehicleInfo (productInstance ^. #_entityId)
  return $
    Trip
      { id = getId prodInstId,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = Just $ GT.mkPrice productInstance,
        route = Nothing
      }

getIdShortIdAndTime :: GuidLike Flow b => Flow (UTCTime, b, Text)
getIdShortIdAndTime = do
  now <- getCurrentTime
  guid <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return (now, guid, shortId)

validateContext :: Text -> Context -> Flow ()
validateContext action context = do
  validateDomain Domain.MOBILITY context
  validateContextCommons action context

mkRideReq :: Id ProductInstance.ProductInstance -> SRideRequest.RideRequestType -> Flow SRideRequest.RideRequest
mkRideReq prodInstID rideRequestType = do
  guid <- generateGUID
  currTime <- getCurrentTime
  pure
    SRideRequest.RideRequest
      { _id = Id guid,
        _rideId = cast prodInstID,
        _createdAt = currTime,
        _type = rideRequestType
      }

makeBppUrl :: Organization.Organization -> BaseUrl -> BaseUrl
makeBppUrl transporterOrg url =
  let orgId = getId $ transporterOrg ^. #_id
      newPath = baseUrlPath url <> "/" <> T.unpack orgId
   in url {baseUrlPath = newPath}
