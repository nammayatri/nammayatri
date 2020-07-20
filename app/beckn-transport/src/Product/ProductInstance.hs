{-# LANGUAGE OverloadedLabels #-}

module Product.ProductInstance where

import App.Types
import Beckn.Types.App as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.ProductInstance as ProdInst
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as V
import Beckn.Utils.Common (encodeToText, fromMaybeM400, withFlowHandler)
import Beckn.Utils.Extra (headMaybe)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Product.BecknProvider.BP as BP
import Servant
import qualified Storage.Queries.Case as CQ
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Person as PersQ
import qualified Storage.Queries.ProductInstance as PIQ
import qualified Storage.Queries.Vehicle as VQ
import qualified Types.API.Case as APICase
import Types.API.ProductInstance
import qualified Utils.Defaults as Default
import qualified Utils.Notifications as Notify

list :: SR.RegistrationToken -> [ProdInst.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
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

update :: SR.RegistrationToken -> Text -> ProdInstUpdateReq -> FlowHandler ProdInstInfo
update SR.RegistrationToken {..} prodInstId req = withFlowHandler $ do
  user <- PersQ.findPersonById (PersonId _EntityId)
  pi <- PIQ.findById (ProductInstanceId prodInstId)
  piList <- PIQ.findAllByParentId (pi ^. #_parentId)
  isAllowed pi req
  vehIdRes <- case req ^. #_vehicleId of
    Just k ->
      when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
        PIQ.updateVeh (ProdInst._id <$> piList) (req ^. #_vehicleId)
    Nothing -> return ()
  dvrIdRes <- case req ^. #_personId of
    Just k ->
      when (user ^. #_role == SP.ADMIN) $
        PIQ.updateDvr (ProdInst._id <$> piList) (PersonId <$> req ^. #_personId)
    Nothing -> return ()
  tripRes <- case req ^. #_status of
    Just c ->
      when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
        updateTrip (ProductInstanceId prodInstId) c
    Nothing -> return ()
  updatedProd <- PIQ.findById (ProductInstanceId prodInstId)
  driverInfo <- case updatedProd ^. #_personId of
    Just driverId -> PersQ.findPersonById driverId
    Nothing -> L.throwException $ err400 {errBody = "DRIVER_ID MISSING"}
  vehicleInfo <- case updatedProd ^. #_entityId of
    Just vehicleId ->
      VQ.findVehicleById (VehicleId vehicleId)
        >>= fromMaybeM400 "VEHICLE NOT FOUND"
    Nothing -> L.throwException $ err400 {errBody = "VEHICLE_ID MISSING"}
  infoObj <- updateInfo (ProductInstanceId prodInstId) (Just driverInfo) (Just vehicleInfo)
  notifyTripDataToGateway pi
  notifyCancelReq updatedProd (req ^. #_status)
  return $ updatedProd {ProdInst._info = infoObj}

notifyTripDataToGateway :: ProdInst.ProductInstance -> Flow ()
notifyTripDataToGateway prodInst = do
  piList <- PIQ.findAllByParentId (prodInst ^. #_parentId)
  cases <- CQ.findAllByIds (ProdInst._caseId <$> piList)
  let trackerCase = headMaybe $ filter (\x -> x ^. #_type == Case.LOCATIONTRACKER) cases
  let parentCase = headMaybe $ filter (\x -> x ^. #_type == Case.RIDESEARCH) cases
  case (trackerCase, parentCase) of
    (Just x, Just y) -> BP.notifyTripInfoToGateway prodInst x y
    _ -> return ()

updateInfo :: ProductInstanceId -> Maybe SP.Person -> Maybe V.Vehicle -> Flow (Maybe Text)
updateInfo prodInstId driverInfo vehicleInfo = do
  let info = Just $ encodeToText (mkInfoObj driverInfo vehicleInfo)
  PIQ.updateInfo prodInstId info
  return info
  where
    mkInfoObj drivInfo vehiInfo =
      DriverVehicleInfo
        { driverInfo = encodeToText drivInfo,
          vehicleInfo = encodeToText vehiInfo
        }

updateTrip :: ProductInstanceId -> ProdInst.ProductInstanceStatus -> Flow ()
updateTrip prodInstId k = do
  prodInst <- PIQ.findById prodInstId
  piList <- PIQ.findAllByParentId (prodInst ^. #_parentId)
  trackerCase_ <- CQ.findByIdType (ProdInst._caseId <$> piList) Case.LOCATIONTRACKER
  parentCase_ <- CQ.findByIdType (ProdInst._caseId <$> piList) Case.RIDESEARCH
  case k of
    ProdInst.CANCELLED -> do
      PIQ.updateStatusByIds (ProdInst._id <$> piList) k
      CQ.updateStatus (Case._id trackerCase_) Case.CLOSED
      return ()
    ProdInst.INPROGRESS -> do
      -- update tracker case and prodinstuct of both cases to INPROGRESS
      PIQ.updateStatusByIds (ProdInst._id <$> piList) k
      CQ.updateStatus (Case._id trackerCase_) Case.INPROGRESS
      return ()
    ProdInst.COMPLETED -> do
      -- update both cases and prodinstucts to COMPLETED
      PIQ.updateStatusByIds (ProdInst._id <$> piList) k
      CQ.updateStatus (Case._id trackerCase_) Case.COMPLETED
      CQ.updateStatus (Case._id parentCase_) Case.COMPLETED
      return ()
    _ -> return ()

notifyCancelReq :: ProdInst.ProductInstance -> Maybe ProdInst.ProductInstanceStatus -> Flow ()
notifyCancelReq prodInst status =
  case status of
    Just k -> case k of
      ProdInst.CANCELLED -> do
        admins <-
          PersQ.findAllByOrgIds [SP.ADMIN] [ProdInst._organizationId prodInst]
        BP.notifyCancelToGateway (_getProductInstanceId $ prodInst ^. #_id)
        Notify.notifyCancelReqByBP prodInst admins
      _ -> return ()
    Nothing -> return ()

listDriverRides :: SR.RegistrationToken -> Text -> FlowHandler RideListRes
listDriverRides SR.RegistrationToken {..} personId = withFlowHandler $ do
  user <- PersQ.findPersonById (PersonId _EntityId)
  person <- PersQ.findPersonById (PersonId personId)
  hasAccess user person
  rideList <- PIQ.findAllByPersonId (SP._id person)
  locList <- LQ.findAllByLocIds (catMaybes (ProdInst._fromLocation <$> rideList)) (catMaybes (ProdInst._toLocation <$> rideList))
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
      find (\x -> ProdInst._fromLocation ride == Just (_getLocationId (Loc._id x))) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> ProdInst._toLocation ride == Just (_getLocationId (Loc._id x))) locList
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
  locList <- LQ.findAllByLocIds (catMaybes (ProdInst._fromLocation <$> rideList)) (catMaybes (ProdInst._toLocation <$> rideList))
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
      find (\x -> ProdInst._fromLocation ride == Just (_getLocationId (Loc._id x))) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> ProdInst._toLocation ride == Just (_getLocationId (Loc._id x))) locList
        prepare ride from to =
          RideRes
            { _product = ride,
              _fromLocation = from,
              _toLocation = to
            }

listCasesByProductInstance :: SR.RegistrationToken -> Text -> Maybe Case.CaseType -> FlowHandler APICase.CaseListRes
listCasesByProductInstance SR.RegistrationToken {..} prodInstId csType = withFlowHandler $ do
  pi <- PIQ.findById (ProductInstanceId prodInstId)
  piList <- PIQ.findAllByParentId (pi ^. #_parentId)
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (ProdInst._caseId <$> piList) type_
    Nothing -> CQ.findAllByIds (ProdInst._caseId <$> piList)
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

isAllowed :: ProdInst.ProductInstance -> ProdInstUpdateReq -> Flow ()
isAllowed pi req = do
  piList <- PIQ.findAllByStatusParentId [ProdInst.COMPLETED, ProdInst.INPROGRESS] (pi ^. #_parentId)
  unless (null piList) $
    case (req ^. #_personId, req ^. #_vehicleId) of
      (Nothing, Nothing) -> return ()
      _ -> L.throwException $ err400 {errBody = "INVALID UPDATE OPERATION"}
