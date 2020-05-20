{-# LANGUAGE OverloadedLabels #-}
module Product.Products where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import Types.API.Products
import Types.API.Case
import qualified Storage.Queries.Case as CQ
import Storage.Queries.Location as LQ
import qualified Storage.Queries.CaseProduct as CPQ
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseP
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.Location as Location
import qualified Types.Storage.Driver as D
import qualified Beckn.Types.Storage.Vehicle as V
import qualified Storage.Queries.Vehicle as VQ
import qualified Storage.Queries.Driver as DQ
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Products as DB
import qualified Beckn.Types.Storage.Products as Storage
import qualified Storage.Queries.RegistrationToken as QR
import           Types.API.CaseProduct
import System.Environment
import qualified Data.Text as T
import Types.App
import Beckn.Utils.Common (encodeToText, withFlowHandler)


update :: Maybe Text -> Text -> ProdReq -> FlowHandler ProdInfoRes
update regToken productId ProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  user <- QP.findPersonById (PersonId _EntityId)
  vehIdRes <- case _vehicleId of
            Just k ->  whenM (return $ (user ^. #_role) == SP.ADMIN || (user ^. #_role) == SP.DRIVER ) $
              DB.updateVeh (ProductsId productId) _vehicleId
            Nothing -> return ()
  dvrIdRes <- case _assignedTo of
            Just k -> whenM (return $ (user ^. #_role) == SP.ADMIN ) $
              DB.updateDvr (ProductsId productId) _assignedTo
            Nothing -> return ()
  tripRes <- case _status of
            Just c -> whenM (return $ (user ^. #_role) == SP.ADMIN || (user ^. #_role) == SP.DRIVER) $
              updateTrip (ProductsId productId) c
            Nothing -> return ()

  updatedProd <- DB.findById (ProductsId productId)
  driverInfo <- case (updatedProd ^. #_assignedTo) of
    Just driverId ->  QP.findPersonById (PersonId driverId)
    Nothing -> L.throwException $ err400 {errBody = "DRIVER_ID MISSING"}
  vehicleInfo <- case (updatedProd ^. #_udf3) of
    Just vehicleId -> VQ.findVehicleById (VehicleId vehicleId)
    Nothing -> return Nothing
  infoObj <- updateInfo (ProductsId productId) (Just driverInfo) vehicleInfo
  return $ updatedProd { Storage._info = infoObj }

updateInfo :: ProductsId -> Maybe SP.Person -> Maybe V.Vehicle  -> L.Flow (Maybe Text)
updateInfo productId driverInfo vehicleInfo = do
  let info = Just $ encodeToText (mkInfoObj driverInfo vehicleInfo)
  DB.updateInfo productId info
  return info
  where
    mkInfoObj drivInfo vehiInfo =
      Storage.ProdInfo
        { driverInfo = encodeToText drivInfo
        , vehicleInfo = encodeToText vehiInfo
        }

updateTrip :: ProductsId -> Product.ProductsStatus -> L.Flow ()
updateTrip productId k = do
  cpList <- CPQ.findAllByProdId productId
  case_ <- CQ.findByIdType (CaseP._caseId <$> cpList) (Case.TRACKER)
  DB.updateStatus productId k
  CQ.updateStatus (Case._id case_) (read (show k) :: Case.CaseStatus)
  CPQ.updateStatus (Case._id case_) productId (read (show k) :: CaseP.CaseProductStatus)
  return ()

listRides :: Maybe Text -> FlowHandler ProdListRes
listRides regToken = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  person <- QP.findPersonById (PersonId _EntityId)
  rideList <- DB.findAllByAssignedTo $ _getPersonId (SP._id person)
  locList <- LQ.findAllByLocIds (catMaybes (Storage._fromLocation <$> rideList)) (catMaybes (Storage._toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    joinByIds locList ride =
      case find (\x -> (Storage._fromLocation ride == Just (_getLocationId (Location._id x)))) locList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare ride k) <$> find (\x -> (Storage._toLocation ride == Just (_getLocationId (Location._id x)))) locList
        prepare ride from to =
          ProdRes
            { _product = ride,
              _fromLocation = from,
              _toLocation = to
            }


listCasesByProd :: Maybe Text -> Text -> Maybe Case.CaseType -> FlowHandler CaseListRes
listCasesByProd regToken productId csType  = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  cpList <- CPQ.findAllByProdId (ProductsId productId)
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (CaseP._caseId <$> cpList) type_
    Nothing -> CQ.findAllByIds (CaseP._caseId <$> cpList)
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