module Product.BecknProvider.BP where

import App.Types (FlowHandler)
import Beckn.Product.Validation.Context
  ( validateContextCommons,
    validateDomain,
  )
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Cancel as API
import qualified Beckn.Types.Core.API.Status as API
import qualified Beckn.Types.Core.API.Track as API
import qualified Beckn.Types.Core.API.Update as API
import Beckn.Types.Core.Ack
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
import qualified ExternalAPI.Flow as ExternalAPI
import qualified ExternalAPI.Transform as ExternalAPITransform
import qualified Storage.Queries.Case as Case
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
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.RideRequest as SRideRequest
import Utils.Common
import qualified Utils.Notifications as Notify

cancel :: Id Organization.Organization -> Organization.Organization -> API.CancelReq -> FlowHandler AckResponse
cancel transporterId _bapOrg req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    let context = req.context
    validateContext "cancel" context
    let prodInstId = req.message.order.id -- transporter search productInstId
    transporterOrg <- Organization.findOrganizationById transporterId
    prodInst <- ProductInstance.findById (Id prodInstId)
    piList <- ProductInstance.findAllByParentId (prodInst.id)
    orderPi <- ProductInstance.findByIdType (ProductInstance.id <$> piList) Case.RIDEORDER
    RideRequest.createFlow =<< mkRideReq (orderPi.id) (transporterOrg.shortId) SRideRequest.CANCELLATION
    return Ack

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride ->
  Bool ->
  m ()
cancelRide rideId requestedByDriver = do
  orderPi <- ProductInstance.findById $ cast rideId
  searchPiId <- ProductInstance.parentId orderPi & fromMaybeM (PIFieldNotPresent "parent_id")
  searchPi <- ProductInstance.findById searchPiId
  piList <- ProductInstance.findAllByParentId searchPiId
  trackerPi <- ProductInstance.findByIdType (ProductInstance.id <$> piList) Case.LOCATIONTRACKER
  cancelRideTransaction piList searchPiId (trackerPi.id) (orderPi.id) requestedByDriver
  fork "cancelRide - Notify BAP" $ do
    let transporterId = ProductInstance.organizationId orderPi
    transporter <- Organization.findOrganizationById transporterId
    notifyCancelToGateway searchPi transporter
    case piList of
      [] -> pure ()
      prdInst : _ -> do
        c <- Case.findById (prdInst.caseId) >>= fromMaybeM CaseNotFound
        case prdInst.personId of
          Nothing -> pure ()
          Just driverId -> do
            driver <- Person.findPersonById driverId
            Notify.notifyDriverOnCancel c driver

cancelRideTransaction ::
  DBFlow m r =>
  [ProductInstance.ProductInstance] ->
  Id ProductInstance.ProductInstance ->
  Id ProductInstance.ProductInstance ->
  Id ProductInstance.ProductInstance ->
  Bool ->
  m ()
cancelRideTransaction piList searchPiId trackerPiId orderPiId requestedByDriver = DB.runSqlDBTransaction $ do
  case piList of
    [] -> pure ()
    (prdInst : _) -> do
      QCase.updateStatusByIds (ProductInstance.caseId <$> piList) Case.CLOSED
      let mbPersonId = prdInst.personId
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
serviceStatus transporterId bapOrg req = withFlowHandlerBecknAPI $ do
  logTagInfo "serviceStatus API Flow" $ show req
  let piId = Id $ req.message.order.id -- transporter search product instance id
  trackerPi <- ProductInstance.findByParentIdType piId Case.LOCATIONTRACKER
  callbackUrl <- bapOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  transporter <- Organization.findOrganizationById transporterId
  let context = req.context
  ExternalAPI.withCallback transporter "status" API.onStatus context callbackUrl $
    mkOnServiceStatusPayload piId trackerPi

notifyServiceStatusToGateway ::
  ( DBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Organization.Organization ->
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstance ->
  m ()
notifyServiceStatusToGateway transporter searchPi trackerPi = do
  mkOnServiceStatusPayload (searchPi.id) trackerPi
    >>= ExternalAPI.callBAP "on_status" API.onStatus transporter (searchPi.caseId) . Right

mkOnServiceStatusPayload :: MonadTime m => Id ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> m API.OnStatusReqMessage
mkOnServiceStatusPayload piId trackerPi = do
  mkOrderRes piId (getId $ trackerPi.productId) (show $ trackerPi.status)
    <&> API.OnStatusReqMessage
  where
    mkOrderRes prodInstId productId status = do
      now <- getCurrentTime
      return $
        Order
          { id = getId prodInstId,
            state = T.pack status,
            items = [OrderItem productId Nothing],
            created_at = now,
            updated_at = now,
            billing = Nothing,
            payment = Nothing,
            update_action = Nothing,
            quotation = Nothing
          }

trackTrip :: Id Organization.Organization -> Organization.Organization -> API.TrackTripReq -> FlowHandler API.TrackTripRes
trackTrip transporterId org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "track trip API Flow" $ show req
    let context = req.context
    validateContext "track" context
    let tripId = req.message.order_id
    trackerCase <- Case.findById (Id tripId) >>= fromMaybeM CaseDoesNotExist
    callbackUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    transporter <- Organization.findOrganizationById transporterId
    ExternalAPI.withCallback transporter "track" API.onTrackTrip context callbackUrl $ do
      let data_url = ExternalAPITransform.baseTrackingUrl <> "/" <> getId (trackerCase.id)
      let tracking = ExternalAPITransform.mkTracking "PULL" data_url
      return $ API.OnTrackReqMessage (Just tracking)

notifyTripInfoToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  ProductInstance.ProductInstance ->
  Id Case.Case ->
  Organization.Organization ->
  Id Case.Case ->
  m ()
notifyTripInfoToGateway prodInst trackerCaseId transporter parentCaseId = do
  mkOnUpdatePayload prodInst trackerCaseId
    >>= ExternalAPI.callBAP "on_update" API.onUpdate transporter parentCaseId . Right

notifyCancelToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  ProductInstance.ProductInstance ->
  Organization.Organization ->
  m ()
notifyCancelToGateway searchPi transporter = do
  mkCancelTripObj searchPi
    >>= ExternalAPI.callBAP "on_cancel" API.onCancel transporter (searchPi.caseId) . Right

mkTrip :: (DBFlow m r, EncFlow m r) => Id Case.Case -> ProductInstance.ProductInstance -> m Trip
mkTrip cId orderPi = do
  prodInst <- ProductInstance.findByCaseId cId
  driver <- mapM mkDriverInfo $ prodInst.personId
  vehicle <- join <$> mapM mkVehicleInfo (prodInst.entityId)
  tripCode <- orderPi.udf4 & fromMaybeM (PIFieldNotPresent "udf4")
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

mkOnUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
  ProductInstance.ProductInstance ->
  Id Case.Case ->
  m API.OnUpdateOrder
mkOnUpdatePayload prodInst caseId = do
  trip <- mkTrip caseId prodInst
  order <- ExternalAPITransform.mkOrder prodInst (Just trip)
  return $ API.OnUpdateOrder order

mkDriverInfo :: (DBFlow m r, EncFlow m r) => Id Person.Person -> m Driver
mkDriverInfo driverId = do
  person <- Person.findPersonById driverId
  ExternalAPITransform.mkDriverObj person

mkVehicleInfo :: DBFlow m r => Text -> m (Maybe BVehicle.Vehicle)
mkVehicleInfo vehicleId = do
  vehicle <- Vehicle.findVehicleById (Id vehicleId)
  return $ ExternalAPITransform.mkVehicleObj <$> vehicle

mkCancelTripObj :: (DBFlow m r, EncFlow m r) => ProductInstance.ProductInstance -> m Trip
mkCancelTripObj productInstance = do
  driver <- mapM mkDriverInfo $ productInstance.personId
  vehicle <- join <$> mapM mkVehicleInfo (productInstance.entityId)
  return $
    Trip
      { id = getId $ productInstance.id,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = Just $ ExternalAPITransform.mkPrice productInstance,
        route = Nothing
      }

getIdShortIdAndTime :: (MonadFlow m, GuidLike m b) => m (UTCTime, b, ShortId a)
getIdShortIdAndTime = do
  now <- getCurrentTime
  guid <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return (now, guid, ShortId shortId)

validateContext :: HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text] => Text -> Context -> m ()
validateContext action context = do
  validateDomain Domain.MOBILITY context
  validateContextCommons action context

mkRideReq ::
  MonadFlow m =>
  Id ProductInstance.ProductInstance ->
  ShortId Organization.Organization ->
  SRideRequest.RideRequestType ->
  m SRideRequest.RideRequest
mkRideReq prodInstID shortOrgId rideRequestType = do
  guid <- generateGUID
  currTime <- getCurrentTime
  pure
    SRideRequest.RideRequest
      { id = Id guid,
        rideId = cast prodInstID,
        shortOrgId = shortOrgId,
        createdAt = currTime,
        _type = rideRequestType
      }
