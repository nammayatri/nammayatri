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
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.CaseProduct as CPQ
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseP
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Person as SP
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


update :: Maybe Text -> ProdReq -> FlowHandler ProdInfoRes
update regToken ProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  case _assignedTo of
    Just _ -> do
      let info = Just $ U.encodeTypeToText (prepareInfo _driverInfo _vehicleInfo)
      DB.updateInfo _productId info _assignedTo
  case _status of
    Just k -> do
      cpList <- CPQ.findAllByProdId _productId
      case_ <- CQ.findByIdType (CaseP._caseId <$> cpList) (Case.TRACKER)
      DB.updateStatus _productId k
      CQ.updateStatus (Case._id case_) (read (show k) :: Case.CaseStatus)
      CPQ.updateStatus (Case._id case_) _productId (read (show k) :: CaseP.CaseProductStatus)
  updatedProd <- DB.findById _productId
  return $ updatedProd
  where
    prepareInfo drivInfo vehiInfo = Storage.ProdInfo
          { driverInfo = U.encodeTypeToText drivInfo
          , vehicleInfo = U.encodeTypeToText vehiInfo
          }

listRides :: Maybe Text -> FlowHandler RideList
listRides regToken = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  person <- QP.findPersonById (PersonId _EntityId)
  rideList <- DB.findAllByAssignedTo $ _getPersonId (SP._id person)
  return $ rideList
