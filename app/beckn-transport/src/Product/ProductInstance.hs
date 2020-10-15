{-# LANGUAGE OverloadedLabels #-}

module Product.ProductInstance where

import App.Types
import Beckn.External.FCM.Types as FCM
import Beckn.Types.App as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as V
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Models.Case as CQ
import Product.BecknProvider.BP as BP
import Servant
import Storage.Queries.Location as LQ
import Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as PersQ
import qualified Storage.Queries.ProductInstance as PIQ
import qualified Storage.Queries.Vehicle as VQ
import qualified Types.API.Case as APICase
import Types.API.ProductInstance
import qualified Utils.Defaults as Default
import qualified Utils.Notifications as Notify

list :: SR.RegistrationToken -> [PI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
list SR.RegistrationToken {..} status csTypes limitM offsetM = withFlowHandler $ do
  person <- PersQ.findPersonById (PersonId _EntityId)
  case SP._organizationId person of
    Just orgId -> do
      result <- PIQ.productInstanceJoin limit offset csTypes orgId status
      locList <- LQ.findAllByLocIds (Case._fromLocationId <$> (_case <$> result)) (Case._toLocationId <$> (_case <$> result))
      return $ buildResponse locList <$> result
    Nothing ->
      L.throwException $ err400 {errBody = "organisation id is missing"}
  where
    limit = fromMaybe Default.limit limitM
    offset = fromMaybe Default.offset offsetM
    buildResponse :: [Loc.Location] -> ProductInstanceRes -> ProductInstanceRes
    buildResponse locList res =
      ProductInstanceRes
        { _case = res ^. #_case,
          _product = res ^. #_product,
          _productInstance = res ^. #_productInstance,
          _fromLocation = find (\x -> Case._fromLocationId (res ^. #_case) == _getLocationId (Loc._id x)) locList,
          _toLocation = find (\x -> Case._toLocationId (res ^. #_case) == _getLocationId (Loc._id x)) locList
        }

update :: SR.RegistrationToken -> ProductInstanceId -> ProdInstUpdateReq -> FlowHandler ProdInstInfo
update SR.RegistrationToken {..} piId req = withFlowHandler $ do
  user <- PersQ.findPersonById (PersonId _EntityId)
  ordPi <- PIQ.findById piId -- order product instance
  searchPi <- case ordPi ^. #_parentId of
    Just pid -> PIQ.findById pid
    Nothing ->
      L.throwException $
        err400 {errBody = "INVALID FLOW"}
  piList <- PIQ.findAllByParentId (ordPi ^. #_parentId)
  isAllowed ordPi req
  updateVehicleDetails user piList req
  updateDriverDetails user piList req
  updateStatus user piId req
  updateInfo piId
  notifyTripDetailsToGateway searchPi ordPi
  notifyStatusUpdateReq searchPi (req ^. #_status)
  PIQ.findById piId

listDriverRides :: SR.RegistrationToken -> Text -> FlowHandler RideListRes
listDriverRides SR.RegistrationToken {..} personId = withFlowHandler $ do
  user <- PersQ.findPersonById (PersonId _EntityId)
  person <- PersQ.findPersonById (PersonId personId)
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
        $ L.throwException $
          err401 {errBody = "Unauthorized"}
    joinByIds locList ride =
      find (\x -> PI._fromLocation ride == Just (_getLocationId (Loc._id x))) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI._toLocation ride == Just (_getLocationId (Loc._id x))) locList
        prepare pRide from to =
          RideRes
            { _product = pRide,
              _fromLocation = from,
              _toLocation = to
            }

listVehicleRides :: SR.RegistrationToken -> Text -> FlowHandler RideListRes
listVehicleRides SR.RegistrationToken {..} vehicleId = withFlowHandler $ do
  user <- PersQ.findPersonById (PersonId _EntityId)
  vehicle <- VQ.findVehicleById (VehicleId vehicleId)
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
        $ L.throwException $
          err401 {errBody = "Unauthorized"}
    joinByIds locList ride =
      find (\x -> PI._fromLocation ride == Just (_getLocationId (Loc._id x))) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI._toLocation ride == Just (_getLocationId (Loc._id x))) locList
        prepare pRide from to =
          RideRes
            { _product = pRide,
              _fromLocation = from,
              _toLocation = to
            }

listCasesByProductInstance :: SR.RegistrationToken -> Text -> Maybe Case.CaseType -> FlowHandler APICase.CaseListRes
listCasesByProductInstance SR.RegistrationToken {..} piId csType = withFlowHandler $ do
  prodInst <- PIQ.findById (ProductInstanceId piId)
  piList <- PIQ.findAllByParentId (prodInst ^. #_parentId)
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (PI._caseId <$> piList) type_
    Nothing -> CQ.findAllByIds (PI._caseId <$> piList)
  locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
  return $ catMaybes $ joinByIds locList <$> caseList
  where
    joinByIds locList cs =
      find (\x -> Case._fromLocationId cs == _getLocationId (Loc._id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case._toLocationId cs == _getLocationId (Loc._id x)) locList
        prepare pcs from to =
          APICase.CaseRes
            { _case = pcs,
              _fromLocation = from,
              _toLocation = to
            }

-- Core Utility methods are below

isAllowed :: PI.ProductInstance -> ProdInstUpdateReq -> Flow ()
isAllowed prodInst req = do
  piList <- PIQ.findAllByStatusParentId [PI.COMPLETED, PI.INPROGRESS] (prodInst ^. #_parentId)
  unless (null piList) $
    case (req ^. #_personId, req ^. #_vehicleId) of
      (Nothing, Nothing) -> return ()
      _ -> L.throwException $ err400 {errBody = "INVALID UPDATE OPERATION"}

updateDriverDetails :: SP.Person -> [PI.ProductInstance] -> ProdInstUpdateReq -> Flow ()
updateDriverDetails user piList req = case req ^. #_personId of
  Just driverId ->
    when (user ^. #_role == SP.ADMIN) $
      case piList of
        [] -> throwJsonError400 "BAD_REQUEST" "INVALID_PRODUCT_INSTANCE_ID"
        p : _ -> do
          PIQ.updateDriver (PI._id <$> piList) (Just $ PersonId driverId)
          driver <- PersQ.findPersonById (PersonId driverId)
          Notify.notifyDriver notificationType notificationTitle (message p) driver
          return ()
  Nothing -> return ()
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message p' =
      unwords
        [ "You have been assigned a ride scheduled for",
          showTimeIst (PI._startTime p') <> ".",
          "Check the app for more details."
        ]

updateVehicleDetails :: SP.Person -> [PI.ProductInstance] -> ProdInstUpdateReq -> Flow ()
updateVehicleDetails user piList req = case req ^. #_vehicleId of
  Just _ ->
    when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
      PIQ.updateVehicle (PI._id <$> piList) (req ^. #_vehicleId)
  Nothing -> return ()

updateStatus :: SP.Person -> ProductInstanceId -> ProdInstUpdateReq -> Flow ()
updateStatus user piId req = do
  prodInst <- PIQ.findById piId
  _ <- case (req ^. #_status, prodInst ^. #_entityId, prodInst ^. #_personId) of
    (Just PI.CANCELLED, _, _) ->
      when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
        updateTrip (prodInst ^. #_id) PI.CANCELLED
    (Just PI.INPROGRESS, Just _, Just _) ->
      when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $ do
        inAppOtpCode <- prodInst ^. #_udf4 & fromMaybeM500 "IN_APP_OTP_MISSING"
        tripOtpCode <- req ^. #_otpCode & fromMaybeM400 "TRIP_OTP_MISSING"
        if inAppOtpCode == tripOtpCode
          then updateTrip (prodInst ^. #_id) PI.INPROGRESS
          else throwJsonError400 "ERR" "INCORRECT_TRIP_OTP"
    (Just c, Just _, Just _) ->
      when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
        updateTrip (prodInst ^. #_id) c
    (Nothing, Just _, Just _) -> return ()
    _ -> L.throwException $ err400 {errBody = "DRIVER_VEHICLE_UNASSIGNED"}
  return ()

notifyTripDetailsToGateway :: PI.ProductInstance -> PI.ProductInstance -> Flow ()
notifyTripDetailsToGateway searchPi orderPi = do
  trackerCase <- CQ.findByParentCaseIdAndType (searchPi ^. #_caseId) Case.LOCATIONTRACKER
  parentCase <- CQ.findById (searchPi ^. #_caseId)
  case (trackerCase, parentCase) of
    (Just x, y) -> BP.notifyTripInfoToGateway orderPi x y
    _ -> return ()

updateInfo :: ProductInstanceId -> Flow ()
updateInfo piId = do
  prodInst <- PIQ.findById piId
  case (prodInst ^. #_personId, prodInst ^. #_entityId) of
    (Just driverId, Just vehicleId) -> do
      driver <- PersQ.findPersonById driverId
      vehicle <-
        VQ.findVehicleById (VehicleId vehicleId)
          >>= fromMaybeM400 "VEHICLE NOT FOUND"
      let info = Just $ encodeToText (mkInfoObj driver vehicle)
      PIQ.updateInfo piId info
      return ()
    (_, _) -> return ()
  where
    mkInfoObj drivInfo vehiInfo =
      DriverVehicleInfo
        { driverInfo = encodeToText drivInfo,
          vehicleInfo = encodeToText vehiInfo
        }

updateTrip :: ProductInstanceId -> PI.ProductInstanceStatus -> Flow ()
updateTrip piId k = do
  prodInst <- PIQ.findById piId
  piList <- PIQ.findAllByParentId (prodInst ^. #_parentId)
  trackerCase_ <- CQ.findByIdType (PI._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase_ <- CQ.findByIdType (PI._caseId <$> piList) Case.RIDEORDER
  case k of
    PI.CANCELLED -> do
      trackerPi <- PIQ.findByIdType (PI._id <$> piList) Case.LOCATIONTRACKER
      orderPi <- PIQ.findByIdType (PI._id <$> piList) Case.RIDEORDER
      searchPi <- tryGetSearchProductInstance prodInst
      _ <- PIQ.updateStatus (PI._id trackerPi) PI.COMPLETED
      _ <- PIQ.updateStatus (PI._id orderPi) PI.CANCELLED
      _ <- PIQ.updateStatus (PI._id searchPi) PI.CANCELLED
      CQ.updateStatus (Case._id trackerCase_) Case.CLOSED
      CQ.updateStatus (Case._id orderCase_) Case.CLOSED
      return ()
    PI.INPROGRESS -> do
      _ <- PIQ.updateStatusByIds (PI._id <$> piList) k
      CQ.updateStatus (Case._id trackerCase_) Case.INPROGRESS
      CQ.updateStatus (Case._id orderCase_) Case.INPROGRESS
      return ()
    PI.COMPLETED -> do
      _ <- PIQ.updateStatusByIds (PI._id <$> piList) k
      CQ.updateStatus (Case._id trackerCase_) Case.COMPLETED
      CQ.updateStatus (Case._id orderCase_) Case.COMPLETED
      return ()
    PI.TRIP_ASSIGNED -> do
      _ <- PIQ.updateStatusByIds (PI._id <$> piList) k
      searchPi <- tryGetSearchProductInstance prodInst
      _ <- PIQ.updateStatus (PI._id searchPi) PI.TRIP_ASSIGNED
      pure ()
    PI.TRIP_REASSIGNMENT -> do
      _ <- PIQ.updateStatusByIds (PI._id <$> piList) k
      -- Clean Driver and Vehicle id?
      searchPi <- tryGetSearchProductInstance prodInst
      _ <- PIQ.updateStatus (PI._id searchPi) PI.TRIP_REASSIGNMENT
      pure ()
    _ -> return ()
  where
    tryGetSearchProductInstance productInstance =
      case productInstance ^. #_parentId of
        Just pid -> PIQ.findById pid
        Nothing -> L.throwException $ err400 {errBody = "INVALID FLOW"} -- TODO make proper error reporting

notifyStatusUpdateReq :: PI.ProductInstance -> Maybe PI.ProductInstanceStatus -> Flow ()
notifyStatusUpdateReq searchPi status =
  case status of
    Just k -> case k of
      PI.CANCELLED -> do
        org <- OQ.findOrganizationById $ OrganizationId $ searchPi ^. #_organizationId
        admins <-
          if org ^. #_enabled
            then PersQ.findAllByOrgIds [SP.ADMIN] [PI._organizationId searchPi]
            else return []
        BP.notifyCancelToGateway (_getProductInstanceId $ searchPi ^. #_id)
        Notify.notifyCancelReqByBP searchPi admins
      _ -> do
        trackerPi <- PIQ.findByParentIdType (Just $ searchPi ^. #_id) Case.LOCATIONTRACKER
        BP.notifyServiceStatusToGateway (_getProductInstanceId $ searchPi ^. #_id) trackerPi
    Nothing -> return ()
