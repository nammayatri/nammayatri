{-# LANGUAGE OverloadedLabels #-}

module Product.ProductInstance where

import App.Types
import Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App as BC
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as V
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Models.Case as CQ
import Product.BecknProvider.BP as BP
import qualified Storage.Queries.Allocation as AQ
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DSQ
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as PersQ
import qualified Storage.Queries.ProductInstance as PIQ
import qualified Storage.Queries.Vehicle as VQ
import qualified Types.API.Case as APICase
import Types.API.ProductInstance
import Types.App
import qualified Utils.Defaults as Default
import qualified Utils.Notifications as Notify

list :: SR.RegistrationToken -> [PI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
list SR.RegistrationToken {..} status csTypes limitM offsetM = withFlowHandler $ do
  person <- PersQ.findPersonById (Id _EntityId)
  case SP._organizationId person of
    Just orgId -> do
      result <- PIQ.productInstanceJoin limit offset csTypes orgId status
      locList <- LQ.findAllByLocIds (Case._fromLocationId <$> (_case <$> result)) (Case._toLocationId <$> (_case <$> result))
      return $ buildResponse locList <$> result
    Nothing ->
      throwErrorMsg400 "MISSING_ORG_ID" "organisation id is missing"
  where
    limit = fromMaybe Default.limit limitM
    offset = fromMaybe Default.offset offsetM
    buildResponse :: [Loc.Location] -> ProductInstanceRes -> ProductInstanceRes
    buildResponse locList res =
      ProductInstanceRes
        { _case = res ^. #_case,
          _product = res ^. #_product,
          _productInstance = res ^. #_productInstance,
          _fromLocation = find (\x -> Case._fromLocationId (res ^. #_case) == getId (Loc._id x)) locList,
          _toLocation = find (\x -> Case._toLocationId (res ^. #_case) == getId (Loc._id x)) locList
        }

update :: SR.RegistrationToken -> Id PI.ProductInstance -> ProdInstUpdateReq -> FlowHandler ProdInstInfo
update SR.RegistrationToken {..} piId req = withFlowHandler $ do
  when (maybe False (`elem` forbiddenStatuses) (req ^. #_status)) $
    throwError400 "BAD_STATUS"
  user <- PersQ.findPersonById (Id _EntityId)
  ordPi <- PIQ.findById piId -- order product instance
  searchPi <- case ordPi ^. #_parentId of
    Just pid -> PIQ.findById pid
    Nothing -> throwError400 "INVALID FLOW"
  isAllowed ordPi req
  when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $ do
    when (user ^. #_role == SP.DRIVER && req ^. #_status == Just PI.CANCELLED) $
      DSQ.updateIdleTime . cast $ user ^. #_id
    updateStatus piId req
  notifyUpdateToBAP searchPi ordPi (req ^. #_status)
  PIQ.findById piId
  where
    forbiddenStatuses = [PI.TRIP_ASSIGNED, PI.TRIP_REASSIGNMENT]

notifyUpdateToBAP :: PI.ProductInstance -> PI.ProductInstance -> Maybe PI.ProductInstanceStatus -> Flow ()
notifyUpdateToBAP searchPi orderPi updatedStatus = do
  -- Send callback to BAP
  bapOrg <- fetchBapOrganization $ orderPi ^. #_caseId
  callbackUrl <- bapOrg ^. #_callbackUrl & fromMaybeM500 "ORG_CALLBACK_URL_NOT_CONFIGURED"
  notifyTripDetailsToGateway searchPi orderPi callbackUrl
  notifyStatusUpdateReq searchPi updatedStatus callbackUrl
  where
    fetchBapOrganization caseId = do
      prodCase <- fetchCase caseId >>= fromMaybeM500 "PRODUCT_INSTANCE_WITH_CASE_NOT_PRESENT"
      bapOrgId <- prodCase ^. #_udf4 & fromMaybeM500 "CASE_DOES_NOT_CONTAIN_BAP_ORG_ID"
      OQ.findOrganizationById $ Id bapOrgId
    fetchCase caseId = do
      prodCase <- QCase.findById caseId
      checkDBError prodCase

listDriverRides :: SR.RegistrationToken -> Text -> FlowHandler RideListRes
listDriverRides SR.RegistrationToken {..} personId = withFlowHandler $ do
  user <- PersQ.findPersonById (Id _EntityId)
  person <- PersQ.findPersonById (Id personId)
  hasAccess user person
  rideList <- PIQ.findAllByPersonId (SP._id person)
  locList <- LQ.findAllByLocIds (catMaybes (PI._fromLocation <$> rideList)) (catMaybes (PI._toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    hasAccess user person =
      when
        ( (user ^. #_role) /= SP.ADMIN && (user ^. #_id) /= (person ^. #_id)
            || (user ^. #_organizationId) /= (person ^. #_organizationId)
        )
        $ throwError401 "UNAUTHORIZED"
    joinByIds locList ride =
      find (\x -> PI._fromLocation ride == Just (getId (Loc._id x))) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI._toLocation ride == Just (getId (Loc._id x))) locList
        prepare pRide from to =
          RideRes
            { _product = pRide,
              _fromLocation = from,
              _toLocation = to
            }

listVehicleRides :: SR.RegistrationToken -> Text -> FlowHandler RideListRes
listVehicleRides SR.RegistrationToken {..} vehicleId = withFlowHandler $ do
  user <- PersQ.findPersonById (Id _EntityId)
  vehicle <- VQ.findVehicleById (Id vehicleId)
  hasAccess user vehicle
  rideList <- PIQ.findAllByVehicleId (Just vehicleId)
  locList <- LQ.findAllByLocIds (catMaybes (PI._fromLocation <$> rideList)) (catMaybes (PI._toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    hasAccess user vehicle =
      when
        ( isNothing (SP._organizationId user)
            || (SP._organizationId user /= (V._organizationId <$> vehicle))
        )
        $ throwError401 "UNAUTHORIZED"
    joinByIds locList ride =
      find (\x -> PI._fromLocation ride == Just (getId (Loc._id x))) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI._toLocation ride == Just (getId (Loc._id x))) locList
        prepare pRide from to =
          RideRes
            { _product = pRide,
              _fromLocation = from,
              _toLocation = to
            }

listCasesByProductInstance :: SR.RegistrationToken -> Text -> Maybe Case.CaseType -> FlowHandler APICase.CaseListRes
listCasesByProductInstance SR.RegistrationToken {..} piId csType = withFlowHandler $ do
  prodInst <- PIQ.findById (Id piId)
  piList <- PIQ.findAllByParentId (prodInst ^. #_parentId)
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (PI._caseId <$> piList) type_
    Nothing -> CQ.findAllByIds (PI._caseId <$> piList)
  locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
  return $ catMaybes $ joinByIds locList <$> caseList
  where
    joinByIds locList cs =
      find (\x -> Case._fromLocationId cs == getId (Loc._id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case._toLocationId cs == getId (Loc._id x)) locList
        prepare pcs from to =
          APICase.CaseRes
            { _case = pcs,
              _fromLocation = from,
              _toLocation = to
            }

-- Core Utility methods are below

isAllowed :: PI.ProductInstance -> ProdInstUpdateReq -> Flow ()
isAllowed orderPi req = do
  newStatus <- fromMaybeM400 "INVALID_UPDATE_OPERATION" (req ^. #_status)
  case PI.validateStatusTransition (orderPi ^. #_status) newStatus of
    Left _ -> throwError400 "INVALID_UPDATE_OPERATION"
    Right _ -> return ()

assignDriver :: Id PI.ProductInstance -> Id Driver -> Flow ()
assignDriver productInstanceId driverId = do
  ordPi <- PIQ.findById productInstanceId
  searchPi <- PIQ.findById =<< fromMaybeM500 "PARENT_PI_NOT_FOUND" (ordPi ^. #_parentId)
  piList <- PIQ.findAllByParentId (ordPi ^. #_parentId)
  headPi <- case piList of
    p : _ -> pure p
    [] -> throwBecknError400 "INVALID_PRODUCT_INSTANCE_ID"
  driver <- PersQ.findPersonById $ cast driverId
  vehicleId <-
    driver ^. #_udf1
      & fromMaybeM400 "DRIVER_HAS_NO_VEHICLE"
      <&> Id
  vehicle <-
    VQ.findVehicleById vehicleId
      >>= fromMaybeM400 "VEHICLE_NOT_FOUND"
  let piIdList = PI._id <$> piList

  DB.runSqlDBTransaction (AQ.assignDriver productInstanceId piIdList vehicle driver)

  notifyUpdateToBAP searchPi ordPi (Just PI.TRIP_ASSIGNED)
  Notify.notifyDriver notificationType notificationTitle (message headPi) driver
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message p' =
      unwords
        [ "You have been assigned a ride for",
          showTimeIst (PI._startTime p') <> ".",
          "Check the app for more details."
        ]

updateStatus :: Id PI.ProductInstance -> ProdInstUpdateReq -> Flow ()
updateStatus piId req = do
  prodInst <- PIQ.findById piId
  _ <- case (req ^. #_status, prodInst ^. #_entityId, prodInst ^. #_personId) of
    (Just PI.CANCELLED, _, _) ->
      updateTrip (prodInst ^. #_id) PI.CANCELLED req
    (Just PI.INPROGRESS, Just _, Just _) -> do
      inAppOtpCode <- prodInst ^. #_udf4 & fromMaybeM500 "IN_APP_OTP_MISSING"
      tripOtpCode <- req ^. #_otpCode & fromMaybeM400 "TRIP_OTP_MISSING"
      if inAppOtpCode == tripOtpCode
        then updateTrip (prodInst ^. #_id) PI.INPROGRESS req
        else throwBecknError400 "INCORRECT_TRIP_OTP"
    (Just c, Just _, Just _) ->
      updateTrip (prodInst ^. #_id) c req
    (Nothing, Just _, Just _) -> return ()
    _ -> throwError400 "DRIVER_VEHICLE_UNASSIGNED"
  return ()

notifyTripDetailsToGateway :: PI.ProductInstance -> PI.ProductInstance -> BaseUrl -> Flow ()
notifyTripDetailsToGateway searchPi orderPi callbackUrl = do
  trackerCase <- CQ.findByParentCaseIdAndType (searchPi ^. #_caseId) Case.LOCATIONTRACKER
  transporter <- OQ.findOrganizationById . Id $ searchPi ^. #_organizationId
  let bppShortId = getShortId $ transporter ^. #_shortId
  parentCase <- CQ.findById (searchPi ^. #_caseId)
  case (trackerCase, parentCase) of
    (Just x, y) -> BP.notifyTripInfoToGateway orderPi x y callbackUrl bppShortId
    _ -> return ()

unAssignDriverInfo :: [PI.ProductInstance] -> ProdInstUpdateReq -> Flow ()
unAssignDriverInfo productInstances request = do
  when (null productInstances) $
    do
      logError
        "unAssignDriverInfo"
        "Can't unassign driver info for null ProductInstance."
      throwBecknError400 "INVALID_PRODUCT_INSTANCE_ID"
  _ <- PIQ.updateVehicleFlow (PI._id <$> productInstances) Nothing
  _ <- PIQ.updateDriverFlow (PI._id <$> productInstances) Nothing
  notifyDriver (request ^. #_personId)
  where
    notifyDriver Nothing = pure ()
    notifyDriver (Just driverId) = do
      let notificationType = FCM.DRIVER_UNASSIGNED
      let notificationTitle = "Driver has refused the ride!"
      let message =
            unwords
              [ "You have refused the ride.",
                "Check the app for more details."
              ]
      driver <- PersQ.findPersonById (Id driverId)
      Notify.notifyDriver notificationType notificationTitle message driver

updateTrip :: Id PI.ProductInstance -> PI.ProductInstanceStatus -> ProdInstUpdateReq -> Flow ()
updateTrip piId newStatus request = do
  prodInst <- PIQ.findById piId
  piList <- PIQ.findAllByParentId (prodInst ^. #_parentId)
  trackerCase_ <- CQ.findByIdType (PI._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase_ <- CQ.findByIdType (PI._caseId <$> piList) Case.RIDEORDER
  case newStatus of
    -- Only admin can send CANCELLED status to cancel ride
    PI.CANCELLED -> do
      trackerPi <- PIQ.findByIdType (PI._id <$> piList) Case.LOCATIONTRACKER
      orderPi <- PIQ.findByIdType (PI._id <$> piList) Case.RIDEORDER
      searchPi <- case prodInst ^. #_parentId of
        Just pid -> PIQ.findById pid
        Nothing -> throwError400 "INVALID FLOW"
      _ <- PIQ.updateStatus (PI._id trackerPi) PI.COMPLETED
      _ <- PIQ.updateStatus (PI._id orderPi) PI.CANCELLED
      _ <- PIQ.updateStatus (PI._id searchPi) PI.CANCELLED
      CQ.updateStatus (Case._id trackerCase_) Case.CLOSED
      CQ.updateStatus (Case._id orderCase_) Case.CLOSED
      updateOnRide (cast <$> PI._personId orderPi) False
    -- Sent by Driver for order reassignment
    PI.TRIP_REASSIGNMENT -> do
      trackerPi <- PIQ.findByIdType (PI._id <$> piList) Case.LOCATIONTRACKER
      orderPi <- PIQ.findByIdType (PI._id <$> piList) Case.RIDEORDER
      _ <- PIQ.updateStatus (PI._id trackerPi) PI.TRIP_REASSIGNMENT
      _ <- PIQ.updateStatus (PI._id orderPi) PI.TRIP_REASSIGNMENT
      _ <- unAssignDriverInfo piList request
      pure ()
    PI.INPROGRESS -> do
      _ <- PIQ.updateStatusByIdsFlow (PI._id <$> piList) newStatus
      CQ.updateStatus (Case._id trackerCase_) Case.INPROGRESS
      CQ.updateStatus (Case._id orderCase_) Case.INPROGRESS
      return ()
    PI.COMPLETED -> do
      _ <- PIQ.updateStatusByIdsFlow (PI._id <$> piList) newStatus
      CQ.updateStatus (Case._id trackerCase_) Case.COMPLETED
      CQ.updateStatus (Case._id orderCase_) Case.COMPLETED
      orderPi <- PIQ.findByIdType (PI._id <$> piList) Case.RIDEORDER
      updateOnRide (cast <$> PI._personId orderPi) False
      whenJust (orderPi ^. #_personId) (DSQ.updateIdleTime . cast)
      return ()
    PI.TRIP_ASSIGNED -> do
      _ <- PIQ.updateStatusByIdsFlow (PI._id <$> piList) PI.TRIP_ASSIGNED
      pure ()
    _ -> return ()
  where
    updateOnRide Nothing _ = pure ()
    updateOnRide (Just personId) status = DriverInformation.updateOnRideFlow personId status

notifyStatusUpdateReq :: PI.ProductInstance -> Maybe PI.ProductInstanceStatus -> BaseUrl -> Flow ()
notifyStatusUpdateReq searchPi status callbackUrl = do
  transporterOrg <- findOrganization
  let bppShortId = getShortId $ transporterOrg ^. #_shortId
  case status of
    Just k -> case k of
      PI.CANCELLED -> do
        admins <- getAdmins transporterOrg
        BP.notifyCancelToGateway (getId $ searchPi ^. #_id) callbackUrl bppShortId
        Notify.notifyCancelReqByBP searchPi admins
      PI.TRIP_REASSIGNMENT -> do
        admins <- getAdmins transporterOrg
        Notify.notifyDriverCancelledRideRequest searchPi admins
        notifyStatusToGateway bppShortId
      _ -> notifyStatusToGateway bppShortId
    Nothing -> return ()
  where
    findOrganization = OQ.findOrganizationById $ Id $ searchPi ^. #_organizationId
    getAdmins transporterOrg = do
      if transporterOrg ^. #_enabled
        then PersQ.findAllByOrgIds [SP.ADMIN] [PI._organizationId searchPi]
        else pure []
    notifyStatusToGateway bppShortId = do
      trackerPi <- PIQ.findByParentIdType (Just $ searchPi ^. #_id) Case.LOCATIONTRACKER
      BP.notifyServiceStatusToGateway (getId $ searchPi ^. #_id) trackerPi callbackUrl bppShortId
