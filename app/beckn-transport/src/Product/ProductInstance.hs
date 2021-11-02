module Product.ProductInstance where

import App.Types
import Beckn.External.Encryption
import Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Mobility.Order (CancellationSource (..))
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import Product.Person (mkPersonRes)
import qualified Storage.Queries.Allocation as AQ
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as PersQ
import qualified Storage.Queries.ProductInstance as PIQ
import Storage.Queries.SearchReqLocation as LQ
import qualified Storage.Queries.Vehicle as VQ
import qualified Types.API.Case as APICase
import Types.API.ProductInstance
import Types.App
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as SO
import qualified Types.Storage.Person as SP
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.Vehicle as V
import Utils.Common
import qualified Utils.Defaults as Default
import qualified Utils.Notifications as Notify

list :: Id SP.Person -> [PI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
list personId status csTypes limitM offsetM = withFlowHandlerAPI $ do
  person <-
    PersQ.findPersonById personId
      >>= fromMaybeM PersonNotFound
  case person.organizationId of
    Just orgId -> do
      result <- PIQ.productInstanceJoin limit offset csTypes orgId status
      locList <- LQ.findAllByLocIds (Case.fromLocationId <$> (_case <$> result)) (Case.toLocationId <$> (_case <$> result))
      buildResponse locList `traverse` result
    Nothing ->
      throwError (PersonFieldNotPresent "organization_id")
  where
    limit = fromMaybe Default.limit limitM
    offset = fromMaybe Default.offset offsetM
    buildResponse :: (DBFlow m r, EncFlow m r) => [Loc.SearchReqLocation] -> ProductInstanceRes -> m ProductInstanceRes
    buildResponse locList res = do
      driver <- (join <$>) $ PersQ.findPersonById `traverse` res.productInstance.personId
      decDriver <- mkPersonRes `traverse` driver
      vehicle <- (join <$>) $ (VQ.findVehicleById . Id) `traverse` (driver >>= (.udf1))
      return $
        ProductInstanceRes
          { _case = res._case,
            product = res.product,
            productInstance = res.productInstance,
            fromLocation = find (\x -> Case.fromLocationId (res._case) == x.id) locList,
            toLocation = find (\x -> Case.toLocationId (res._case) == x.id) locList,
            driver = decDriver,
            vehicle = vehicle
          }

notifyUpdateToBAP ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  PI.ProductInstance ->
  PI.ProductInstance ->
  PI.ProductInstanceStatus ->
  m ()
notifyUpdateToBAP searchPi orderPi updatedStatus = do
  -- Send callbacks to BAP
  transporter <-
    OQ.findOrganizationById searchPi.organizationId
      >>= fromMaybeM OrgNotFound
  notifyTripDetailsToGateway transporter searchPi orderPi
  notifyStatusUpdateReq transporter searchPi updatedStatus

listDriverRides ::
  Id SP.Person ->
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler RideListRes
listDriverRides personId driverId limit offset = withFlowHandlerAPI $ do
  user <- PersQ.findPersonById personId >>= fromMaybeM PersonNotFound
  person <- PersQ.findPersonById driverId >>= fromMaybeM PersonDoesNotExist
  hasAccess user person
  map toRideRes <$> PIQ.findAllRidesWithLocationsByDriverId (fromMaybe 100 limit) (fromMaybe 0 offset) person.id
  where
    hasAccess user person =
      when
        ( (user.role) /= SP.ADMIN && (user.id) /= (person.id)
            || (user.organizationId) /= (person.organizationId)
        )
        $ throwError Unauthorized
    toRideRes (ride, from, to) =
      RideRes
        { product = ride,
          fromLocation = from,
          toLocation = to
        }

listVehicleRides :: Id SP.Person -> Id V.Vehicle -> FlowHandler RideListRes
listVehicleRides personId vehicleId = withFlowHandlerAPI $ do
  user <- PersQ.findPersonById personId >>= fromMaybeM PersonNotFound
  vehicle <- VQ.findVehicleById vehicleId
  hasAccess user vehicle
  rideList <- PIQ.findAllByVehicleId (Just vehicleId)
  locList <- LQ.findAllByLocIds (catMaybes (PI.fromLocation <$> rideList)) (catMaybes (PI.toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    hasAccess user vehicle =
      when
        ( isNothing user.organizationId
            || (user.organizationId /= (V.organizationId <$> vehicle))
        )
        $ throwError Unauthorized
    joinByIds locList ride =
      find (\x -> PI.fromLocation ride == Just (Loc.id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI.toLocation ride == Just (Loc.id x)) locList
        prepare pRide from to =
          RideRes
            { product = pRide,
              fromLocation = from,
              toLocation = to
            }

listCasesByProductInstance :: Id SP.Person -> Id PI.ProductInstance -> Maybe Case.CaseType -> FlowHandler APICase.CaseListRes
listCasesByProductInstance _ piId csType = withFlowHandlerAPI $ do
  prodInst <- PIQ.findById piId >>= fromMaybeM PIDoesNotExist
  piList <-
    prodInst.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
      >>= PIQ.findAllByParentId
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (PI.caseId <$> piList) type_
    Nothing -> CQ.findAllByIds (PI.caseId <$> piList)
  locList <- LQ.findAllByLocIds (Case.fromLocationId <$> caseList) (Case.toLocationId <$> caseList)
  return $ catMaybes $ joinByIds locList <$> caseList
  where
    joinByIds locList cs =
      find (\x -> Case.fromLocationId cs == Loc.id x) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case.toLocationId cs == Loc.id x) locList
        prepare pcs from to =
          APICase.CaseRes
            { _case = pcs,
              fromLocation = from,
              toLocation = to
            }

-- Core Utility methods are below

assignDriver ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id PI.ProductInstance ->
  Id Driver ->
  m ()
assignDriver productInstanceId driverId = do
  ordPi <- PIQ.findById productInstanceId >>= fromMaybeM PIDoesNotExist
  searchPIId <- ordPi.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  searchPi <- PIQ.findById searchPIId >>= fromMaybeM PINotFound
  piList <-
    ordPi.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
      >>= PIQ.findAllByParentId
  driver <-
    PersQ.findPersonById (cast driverId)
      >>= fromMaybeM PersonDoesNotExist
  vehicleId <-
    driver.udf1
      & fromMaybeM (PersonFieldNotPresent "udf1 - vehicle")
      <&> Id
  vehicle <-
    VQ.findVehicleById vehicleId
      >>= fromMaybeM VehicleNotFound
  let piIdList = PI.id <$> piList
  decDriver <- decrypt driver
  DB.runSqlDBTransaction (AQ.assignDriver piIdList vehicle decDriver)

  fork "assignDriver - Notify BAP" $ do
    uOrdPi <- PIQ.findById productInstanceId >>= fromMaybeM PIDoesNotExist
    notifyUpdateToBAP searchPi uOrdPi PI.TRIP_ASSIGNED
    Notify.notifyDriver notificationType notificationTitle (message uOrdPi) driver.id driver.deviceToken
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message p' =
      unwords
        [ "You have been assigned a ride for",
          showTimeIst (PI.startTime p') <> ".",
          "Check the app for more details."
        ]

notifyTripDetailsToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SO.Organization ->
  PI.ProductInstance ->
  PI.ProductInstance ->
  m ()
notifyTripDetailsToGateway transporter searchPi orderPi = do
  BP.notifyTripInfoToGateway orderPi transporter (searchPi.caseId)

notifyStatusUpdateReq ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SO.Organization ->
  PI.ProductInstance ->
  PI.ProductInstanceStatus ->
  m ()
notifyStatusUpdateReq transporterOrg searchPi status = do
  case status of
    PI.CANCELLED -> do
      admins <- getAdmins
      BP.notifyCancelToGateway searchPi transporterOrg ByOrganization
      Notify.notifyCancelReqByBP searchPi admins
    PI.TRIP_REASSIGNMENT -> do
      admins <- getAdmins
      Notify.notifyDriverCancelledRideRequest searchPi admins
      notifyStatusToGateway
    _ -> notifyStatusToGateway
  where
    getAdmins = do
      if transporterOrg.enabled
        then PersQ.findAllByOrgId [SP.ADMIN] $ PI.organizationId searchPi
        else pure []
    notifyStatusToGateway = do
      trackerPi <-
        PIQ.findByParentIdType (searchPi.id) Case.LOCATIONTRACKER
          >>= fromMaybeM PINotFound
      BP.notifyServiceStatusToGateway transporterOrg searchPi trackerPi
