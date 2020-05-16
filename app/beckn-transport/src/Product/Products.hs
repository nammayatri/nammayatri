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
import qualified Storage.Queries.Driver as VD
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Products as DB
import qualified Beckn.Types.Storage.Products as Storage
import qualified Storage.Queries.RegistrationToken as QR
import           Types.API.CaseProduct
import System.Environment
import qualified Data.Text as T
import Types.App
import Utils.Utils as U
import Beckn.Utils.Common (withFlowHandler)


update :: Maybe Text -> Text -> ProdReq -> FlowHandler ProdInfoRes
update regToken productId ProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  infoRes <- case _driverInfo of
            Just k -> updateInfo (ProductsId productId) _driverInfo _vehicleInfo
            Nothing -> return $ "NO CHANGE"
  idsRes <- case _assignedTo of
            Just k -> updateIds (ProductsId productId) _assignedTo _vehicleId
            Nothing -> return $ "NO CHANGE"
  tripRes <- case _status of
            Just c -> updateTrip (ProductsId productId) c
            Nothing -> return $ "NO CHANGE"
  updatedProd <- DB.findById (ProductsId productId)
  return $ updatedProd

updateInfo :: ProductsId -> Maybe D.Driver -> Maybe V.Vehicle  -> L.Flow Text
updateInfo productId driverInfo vehicleInfo = do
  let info = Just $ U.encodeTypeToText (prepareInfo driverInfo vehicleInfo)
  DB.updateInfo productId info
  return "INFO UPDATED"
  where
    prepareInfo drivInfo vehiInfo = Storage.ProdInfo
          { driverInfo = U.encodeTypeToText drivInfo
          , vehicleInfo = U.encodeTypeToText vehiInfo
          }

-- update udf3 for vehicleId
updateIds :: ProductsId -> Maybe Text -> Maybe Text -> L.Flow Text
updateIds productId assignedTo vehId = do
  DB.updateIds productId assignedTo vehId
  return "IDs UPDATED"

updateTrip :: ProductsId -> Product.ProductsStatus -> L.Flow Text
updateTrip productId k = do
  cpList <- CPQ.findAllByProdId productId
  case_ <- CQ.findByIdType (CaseP._caseId <$> cpList) (Case.TRACKER)
  DB.updateStatus productId k
  CQ.updateStatus (Case._id case_) (read (show k) :: Case.CaseStatus)
  CPQ.updateStatus (Case._id case_) productId (read (show k) :: CaseP.CaseProductStatus)
  return "UPDATED"

listRides :: Maybe Text -> FlowHandler RideList
listRides regToken = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  person <- QP.findPersonById (PersonId _EntityId)
  rideList <- DB.findAllByAssignedTo $ _getPersonId (SP._id person)
  return $ rideList



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