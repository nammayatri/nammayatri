module Product.Case.CRUD where

import Beckn.Types.App
import Beckn.Types.Common as BC
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.CaseProduct as CaseP
import Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import Storage.Queries.Case as Case
import Storage.Queries.CaseProduct as CPQ
import Storage.Queries.Products as PQ
import System.Environment
import Types.API.Case
import Types.API.Registration
import qualified Utils.Defaults as Defaults

list :: CaseReq -> FlowHandler CaseListRes
list CaseReq {..} = withFlowHandler $ do
  Case.findAllByType _limit _offset _type _status

-- Update Case
-- Transporter Accepts a Ride with Quote
-- TODO fromLocation toLocation getCreatedTimeFromInput
update :: Text -> UpdateCaseReq -> FlowHandler Case
update caseId UpdateCaseReq {..} = withFlowHandler $ do
  c <- Case.findById $ CaseId caseId
  case _transporterChoice of
    "ACCEPTED" -> do
      p <- createProduct c _quote Defaults.localTime
      cp <- createCaseProduct c p
      notifyGateway c
      return c
    "DECLINED" -> return c

createProduct :: Case -> Maybe Double -> LocalTime -> L.Flow Products
createProduct cs price ctime = do
  prodId <- L.generateGUID
  (currTime :: LocalTime) <- getCurrTime
  let product = getProduct prodId price cs ctime currTime
  PQ.create product
  return $ product
  where
    getProduct prodId price cs ctime currTime =
      Products
        { _id = ProductsId prodId,
          _name = Case._name cs,
          _description = Case._description cs,
          _industry = read (show (Case._industry cs)) :: ProductsIndustry,
          _type = read (show (Case._type cs)) :: ProductsType,
          _status = read (show (Case._status cs)) :: ProductsStatus,
          _startTime = Case._startTime cs,
          _endTime = Case._endTime cs,
          _validTill = Case._validTill cs,
          _price = fromMaybe 0 price,
          _rating = Nothing,
          _review = Nothing,
          _udf1 = Case._udf1 cs,
          _udf2 = Case._udf2 cs,
          _udf3 = Case._udf3 cs,
          _udf4 = Case._udf4 cs,
          _udf5 = Case._udf5 cs,
          _info = Case._info cs,
          _organizationId = Defaults.orgId,
          _createdAt = ctime,
          _updatedAt = currTime,
          _fromLocation = Nothing,
          _toLocation = Nothing
        }

createCaseProduct :: Case -> Products -> L.Flow CaseProduct
createCaseProduct cs prod = do
  cpId <- L.generateGUID
  (currTime :: LocalTime) <- getCurrTime
  let caseProd = getCaseProd cpId cs prod currTime
  CPQ.create caseProd
  return $ caseProd
  where
    getCaseProd cpId cs prod currTime =
      CaseProduct
        { _id = CaseProductId cpId,
          _caseId = Case._id cs,
          _productId = Product._id prod,
          _quantity = 1,
          _price = Product._price prod,
          _status = read (show (Product._status prod)) :: CaseProductStatus,
          _info = Nothing,
          _createdAt = Case._createdAt cs,
          _updatedAt = currTime
        }

notifyGateway :: Case -> L.Flow ()
notifyGateway _ = undefined
