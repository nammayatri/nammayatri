{-# LANGUAGE OverloadedLabels #-}

module Product.Products where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.ProductInstance as ProdInst
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as V
import Beckn.Utils.Common (encodeToText, fromMaybeM400, withFlowHandler)
import Beckn.Utils.Extra (headMaybe)
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Product.BecknProvider.BP as BP
import Servant
import qualified Storage.Queries.Case as CQ
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Person as PersQ
import qualified Storage.Queries.ProductInstance as CPQ
import qualified Storage.Queries.Products as PQ
import qualified Storage.Queries.RegistrationToken as RQ
import qualified Storage.Queries.Vehicle as VQ
import System.Environment
import Types.API.Case
import Types.API.ProductInstance
import Types.API.Products
import Types.App

update :: RegToken -> Text -> ProdReq -> FlowHandler ProdInfoRes
update regToken productId req = withFlowHandler $ do
  SR.RegistrationToken {..} <- RQ.verifyToken regToken
  user <- PersQ.findPersonById (PersonId _EntityId)
  isAllowed productId req
  vehIdRes <- case req ^. #_vehicleId of
    Just k ->
      when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
        PQ.updateVeh (ProductsId productId) (req ^. #_vehicleId)
    Nothing -> return ()
  dvrIdRes <- case req ^. #_assignedTo of
    Just k ->
      when (user ^. #_role == SP.ADMIN) $
        PQ.updateDvr (ProductsId productId) (req ^. #_assignedTo)
    Nothing -> return ()
  tripRes <- case req ^. #_status of
    Just c ->
      when (user ^. #_role == SP.ADMIN || user ^. #_role == SP.DRIVER) $
        updateTrip (ProductsId productId) c
    Nothing -> return ()
  updatedProd <- PQ.findById (ProductsId productId)
  driverInfo <- case (updatedProd ^. #_assignedTo) of
    Just driverId -> PersQ.findPersonById (PersonId driverId)
    Nothing -> L.throwException $ err400 {errBody = "DRIVER_ID MISSING"}
  vehicleInfo <- case (updatedProd ^. #_udf3) of
    Just vehicleId ->
      VQ.findVehicleById (VehicleId vehicleId)
        >>= fromMaybeM400 "VEHICLE NOT FOUND"
    Nothing -> L.throwException $ err400 {errBody = "VEHICLE_ID MISSING"}
  infoObj <- updateInfo (ProductsId productId) (Just driverInfo) (Just vehicleInfo)
  notifyTripDataToGateway (ProductsId productId)
  notifyCancelReq productId (req ^. #_status)
  return $ updatedProd {Product._info = infoObj}

notifyTripDataToGateway :: ProductsId -> L.Flow ()
notifyTripDataToGateway productId = do
  cps <- CPQ.findAllByProdId productId
  cases <- CQ.findAllByIds (ProdInst._caseId <$> cps)
  let trackerCase = headMaybe $ filter (\x -> x ^. #_type == Case.TRACKER) cases
  let parentCase = headMaybe $ filter (\x -> x ^. #_type == Case.RIDEBOOK) cases
  case (trackerCase, parentCase) of
    (Just x, Just y) -> BP.notifyTripUrlToGateway x y
    _ -> return ()

updateInfo :: ProductsId -> Maybe SP.Person -> Maybe V.Vehicle -> L.Flow (Maybe Text)
updateInfo productId driverInfo vehicleInfo = do
  let info = Just $ encodeToText (mkInfoObj driverInfo vehicleInfo)
  PQ.updateInfo productId info
  return info
  where
    mkInfoObj drivInfo vehiInfo =
      Product.ProdInfo
        { driverInfo = encodeToText drivInfo,
          vehicleInfo = encodeToText vehiInfo
        }

updateTrip :: ProductsId -> ProdInst.ProductInstanceStatus -> L.Flow ()
updateTrip productId k = do
  cpList <- CPQ.findAllByProdId productId
  trackerCase_ <- CQ.findByIdType (ProdInst._caseId <$> cpList) (Case.TRACKER)
  parentCase_ <- CQ.findByIdType (ProdInst._caseId <$> cpList) (Case.RIDEBOOK)
  case k of
    ProdInst.CANCELLED -> do
      CPQ.updateStatusByIds (ProdInst._id <$> cpList) k
      CQ.updateStatus (Case._id trackerCase_) Case.CLOSED
      return ()
    ProdInst.INPROGRESS -> do
      -- update tracker case and prodinstuct of both cases to INPROGRESS
      CPQ.updateStatusByIds (ProdInst._id <$> cpList) k
      CQ.updateStatus (Case._id trackerCase_) Case.INPROGRESS
      return ()
    ProdInst.COMPLETED -> do
      -- update both cases and prodinstucts to COMPLETED
      CPQ.updateStatusByIds (ProdInst._id <$> cpList) k
      CQ.updateStatus (Case._id trackerCase_) Case.COMPLETED
      CQ.updateStatus (Case._id parentCase_) Case.COMPLETED
      return ()
    _ -> return ()

listRides :: RegToken -> Maybe Text -> FlowHandler ProdListRes
listRides regToken vehicleIdM = withFlowHandler $ do
  SR.RegistrationToken {..} <- RQ.verifyToken regToken
  person <- PersQ.findPersonById (PersonId _EntityId)
  whenM (validateOrg vehicleIdM person) $ L.throwException $ err401 {errBody = "Unauthorized"}
  rideList <- case vehicleIdM of
    Just _ -> PQ.findAllByVehicleId vehicleIdM
    Nothing -> PQ.findAllByAssignedTo $ _getPersonId (SP._id person)
  locList <- LQ.findAllByLocIds (catMaybes (Product._fromLocation <$> rideList)) (catMaybes (Product._toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    validateOrg vehicleM person = do
      case vehicleM of
        Just vehicleId -> do
          vehicle <- VQ.findVehicleById (VehicleId vehicleId)
          if isJust (SP._organizationId person) && (SP._organizationId person == (V._organizationId <$> vehicle))
            then return False
            else return True
        Nothing -> return False
    joinByIds locList ride =
      case find (\x -> (Product._fromLocation ride == Just (_getLocationId (Location._id x)))) locList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare ride k) <$> find (\x -> (Product._toLocation ride == Just (_getLocationId (Location._id x)))) locList
        prepare ride from to =
          ProdRes
            { _product = ride,
              _fromLocation = from,
              _toLocation = to
            }

listCasesByProd :: RegToken -> Text -> Maybe Case.CaseType -> FlowHandler CaseListRes
listCasesByProd regToken productId csType = withFlowHandler $ do
  SR.RegistrationToken {..} <- RQ.verifyToken regToken
  cpList <- CPQ.findAllByProdId (ProductsId productId)
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (ProdInst._caseId <$> cpList) type_
    Nothing -> CQ.findAllByIds (ProdInst._caseId <$> cpList)
  locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
  return $ catMaybes $ joinByIds locList <$> caseList
  where
    joinByIds locList cs =
      case find (\x -> (Case._fromLocationId cs == _getLocationId (Location._id x))) locList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare cs k) <$> find (\x -> (Case._toLocationId cs == _getLocationId (Location._id x))) locList
        prepare cs from to =
          CaseRes
            { _case = cs,
              _fromLocation = from,
              _toLocation = to
            }

notifyCancelReq :: Text -> Maybe ProdInst.ProductInstanceStatus -> L.Flow ()
notifyCancelReq prodId status = do
  case status of
    Just k -> case k of
      ProdInst.CANCELLED -> BP.notifyCancelToGateway prodId
      _ -> return ()
    Nothing -> return ()

-- Core Utility methods are below

isAllowed :: Text -> ProdReq -> L.Flow ()
isAllowed productId req = do
  piList <- CPQ.findAllByStatusIds [ProdInst.COMPLETED, ProdInst.INPROGRESS] [ProductsId productId]
  unless (null piList) $
    case (req ^. #_assignedTo, req ^. #_vehicleId) of
      (Nothing, Nothing) -> return ()
      _ -> L.throwException $ err400 {errBody = "INVALID UPDATE OPERATION"}
