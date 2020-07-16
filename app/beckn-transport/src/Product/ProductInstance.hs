{-# LANGUAGE OverloadedLabels #-}

module Product.ProductInstance where

import App.Types
import Beckn.Types.App as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as V
import Beckn.Utils.Common (encodeToText, fromMaybeM400, withFlowHandler)
import Beckn.Utils.Extra (headMaybe)
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Product.BecknProvider.BP as BP
import Servant
import qualified Storage.Queries.Case as CQ
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Person as PersQ
import qualified Storage.Queries.ProductInstance as PIQ
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Vehicle as VQ
import System.Environment
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
  pi <- PIQ.findById piId -- order product instance
  searchPi <- case pi ^. #_parentId of
    Just id -> PIQ.findById id
    Nothing ->
      L.throwException $
        err400 {errBody = "INVALID FLOW"}
  piList <- PIQ.findAllByParentId (pi ^. #_parentId)
  isAllowed pi req
  updateVehicleDetails user piList req
  updateDriverDetails user piList req
  updateStatus user piId req
  updateInfo piId
  notifyTripDetailsToGateway piId
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
        prepare ride from to =
          RideRes
            { _product = ride,
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
        prepare ride from to =
          RideRes
            { _product = ride,
              _fromLocation = from,
              _toLocation = to
            }

listCasesByProductInstance :: SR.RegistrationToken -> Text -> Maybe Case.CaseType -> FlowHandler APICase.CaseListRes
listCasesByProductInstance SR.RegistrationToken {..} piId csType = withFlowHandler $ do
  pi <- PIQ.findById (ProductInstanceId piId)
  piList <- PIQ.findAllByParentId (pi ^. #_parentId)
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
        prepare cs from to =
          APICase.CaseRes
            { _case = cs,
              _fromLocation = from,
              _toLocation = to
            }

-- Core Utility methods are below

isAllowed :: PI.ProductInstance -> ProdInstUpdateReq -> Flow ()
isAllowed pi req = do
  piList <- PIQ.findAllByStatusParentId [PI.COMPLETED, PI.INPROGRESS] (pi ^. #_parentId)
  unless (null piList) $
    case (req ^. #_personId, req ^. #_vehicleId) of
      (Nothing, Nothing) -> return ()
      _ -> L.throwException $ err400 {errBody = "INVALID UPDATE OPERATION"}

updateDriverDetails :: SP.Person -> [PI.ProductInstance] -> ProdInstUpdateReq -> Flow ()
updateDriverDetails user piList req = case req ^. #_personId of
  Just k ->
    when (user ^. #_role == SP.ADMIN) $
      PIQ.updateDriver (PI._id <$> piList) (PersonId <$> req ^. #_personId)
  Nothing -> return ()

updateVehicleDetails :: SP.Person -> [PI.ProductInstance] -> ProdInstUpdateReq -> Flow ()
updateVehicleDetails user piList req = case req ^. #_vehicleId of
  Just k ->
    when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
      PIQ.updateVehicle (PI._id <$> piList) (req ^. #_vehicleId)
  Nothing -> return ()

updateStatus :: SP.Person -> ProductInstanceId -> ProdInstUpdateReq -> Flow ()
updateStatus user piId req = do
  pi <- PIQ.findById piId
  updateRes <- case (req ^. #_status, pi ^. #_entityId, pi ^. #_personId) of
    (Just c, Just x, Just y) ->
      when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
        updateTrip (pi ^. #_id) c
    (Nothing, Just x, Just y) -> return ()
    _ -> L.throwException $ err400 {errBody = "DRIVER_VEHICLE_UNASSIGNED"}
  return ()

notifyTripDetailsToGateway :: ProductInstanceId -> Flow ()
notifyTripDetailsToGateway piId = do
  pi <- PIQ.findById piId
  piList <- PIQ.findAllByParentId (pi ^. #_parentId)
  cases <- CQ.findAllByIds (PI._caseId <$> piList)
  let trackerCase = headMaybe $ filter (\x -> x ^. #_type == Case.LOCATIONTRACKER) cases
  let parentCase = headMaybe $ filter (\x -> x ^. #_type == Case.RIDESEARCH) cases
  case (trackerCase, parentCase) of
    (Just x, Just y) -> BP.notifyTripUrlToGateway x y
    _ -> return ()

updateInfo :: ProductInstanceId -> Flow ()
updateInfo piId = do
  pi <- PIQ.findById piId
  driverInfo <- case pi ^. #_personId of
    Just driverId -> PersQ.findPersonById driverId
    Nothing -> L.throwException $ err400 {errBody = "DRIVER_ID MISSING"}
  vehicleInfo <- case pi ^. #_entityId of
    Just vehicleId ->
      VQ.findVehicleById (VehicleId vehicleId)
        >>= fromMaybeM400 "VEHICLE NOT FOUND"
    Nothing -> L.throwException $ err400 {errBody = "VEHICLE_ID MISSING"}
  let info = Just $ encodeToText (mkInfoObj driverInfo vehicleInfo)
  PIQ.updateInfo piId info
  return ()
  where
    mkInfoObj drivInfo vehiInfo =
      DriverVehicleInfo
        { driverInfo = encodeToText drivInfo,
          vehicleInfo = encodeToText vehiInfo
        }

updateTrip :: ProductInstanceId -> PI.ProductInstanceStatus -> Flow ()
updateTrip piId k = do
  pi <- PIQ.findById piId
  piList <- PIQ.findAllByParentId (pi ^. #_parentId)
  trackerCase_ <- CQ.findByIdType (PI._caseId <$> piList) Case.LOCATIONTRACKER
  orderCase_ <- CQ.findByIdType (PI._caseId <$> piList) Case.RIDEORDER
  case k of
    PI.CANCELLED -> do
      trackerPi <- PIQ.findByIdType (PI._id <$> piList) Case.LOCATIONTRACKER
      orderPi <- PIQ.findByIdType (PI._id <$> piList) Case.RIDEORDER
      PIQ.updateStatus (PI._id trackerPi) PI.COMPLETED
      PIQ.updateStatus (PI._id orderPi) PI.CANCELLED
      CQ.updateStatus (Case._id trackerCase_) Case.CLOSED
      CQ.updateStatus (Case._id orderCase_) Case.CLOSED
      return ()
    PI.INPROGRESS -> do
      PIQ.updateStatusByIds (PI._id <$> piList) k
      CQ.updateStatus (Case._id trackerCase_) Case.INPROGRESS
      CQ.updateStatus (Case._id orderCase_) Case.INPROGRESS
      return ()
    PI.COMPLETED -> do
      PIQ.updateStatusByIds (PI._id <$> piList) k
      CQ.updateStatus (Case._id trackerCase_) Case.COMPLETED
      CQ.updateStatus (Case._id orderCase_) Case.COMPLETED
      return ()
    _ -> return ()

notifyStatusUpdateReq :: PI.ProductInstance -> Maybe PI.ProductInstanceStatus -> Flow ()
notifyStatusUpdateReq searchPi status =
  case status of
    Just k -> case k of
      PI.CANCELLED -> do
        admins <-
          PersQ.findAllByOrgIds [SP.ADMIN] [PI._organizationId searchPi]
        BP.notifyCancelToGateway (_getProductInstanceId $ searchPi ^. #_id)
        Notify.notifyCancelReqByBP searchPi admins
      _ -> do
        trackerPi <- PIQ.findByParentIdType (Just $ searchPi ^. #_id) Case.LOCATIONTRACKER
        BP.notifyServiceStatusToGateway (_getProductInstanceId $ searchPi ^. #_id) trackerPi
    Nothing -> return ()
