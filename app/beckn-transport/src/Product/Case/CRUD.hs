module Product.Case.CRUD where

import           Beckn.Types.App
import           Beckn.Types.Common as BC
import           Beckn.Types.Storage.Case as Case
import           Beckn.Types.Storage.CaseProduct as CaseP
import           Beckn.Types.Storage.Products as Product
import           Storage.Queries.Products as PQ
import           Storage.Queries.CaseProduct as CPQ
import qualified Data.Accessor as Lens
import           Data.Aeson
import qualified Data.Text as T
import           Data.Time.LocalTime
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import           Servant
import qualified Epass.Utils.Defaults as Defaults
import           Storage.Queries.Case as Case
import           System.Environment
import           Types.API.Case
import           Types.API.Registration
import           Types.App
import           Utils.Routes


list :: CaseReq -> FlowHandler CaseListRes
list CaseReq {..} = withFlowHandler $ do
  Case.findAllByType _limit _offset _type _status

-- Update Case
-- Transporter Accepts a Ride with Quote
update :: Text -> UpdateCaseReq -> FlowHandler Case
update caseId UpdateCaseReq {..} = withFlowHandler $ do
  c <- Case.findById $ CaseId caseId
  case _transporterChoice of
    "ACCEPTED" -> do
      p   <- createProduct c _quote
      cp  <- createCaseProduct c p
      notifyGateway c
      return c
    "DECLINED" -> return c

createProduct :: Case -> Maybe Double -> L.Flow Products
createProduct cs price = do
  prodId <- L.generateGUID
  let product = getProduct prodId price cs
  PQ.create product
  return $ product
  where
    getProduct prodId price cs = Products
      { _id = ProductsId prodId
      , _name =  Case._name cs
      , _description = Case._description cs
      , _industry =  read (show (Case._industry cs)) :: ProductsIndustry
      , _type = read (show (Case._type cs)) :: ProductsType
      , _status = read (show (Case._status cs)) :: ProductsStatus
      , _startTime = Case._startTime cs
      , _endTime = Case._endTime cs
      , _validTill = Case._validTill cs
      , _price =  fromMaybe 0 price
      , _rating =  Nothing
      , _review =  Nothing
      , _udf1 = Case._udf1 cs
      , _udf2 = Case._udf2 cs
      , _udf3 = Case._udf3 cs
      , _udf4 = Case._udf4 cs
      , _udf5 = Case._udf5 cs
      , _info = Case._info cs
      , _organizationId =  Defaults.orgId
      , _createdAt = Case._createdAt cs
      , _updatedAt = Case._updatedAt cs
      , _fromLocation =  Nothing
      , _toLocation =  Nothing
      }

createCaseProduct :: Case -> Products -> L.Flow CaseProduct
createCaseProduct cs prod = do
    cpId <- L.generateGUID
    let caseProd = getCaseProd cpId cs prod
    CPQ.create caseProd
    return $ caseProd
    where
      getCaseProd cpId cs prod = CaseProduct
        { _id = CaseProductId cpId
        , _caseId = Case._id cs
        , _productId = Product._id prod
        , _quantity = 1
        , _price = Product._price prod
        , _status = read (show (Product._status prod)) :: CaseProductStatus
        , _info = Nothing
        , _createdAt = Case._createdAt cs
        , _updatedAt = Case._updatedAt cs
        }

notifyGateway :: Case -> L.Flow ()
notifyGateway _ = undefined