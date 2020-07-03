module Product.ProductInstance where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as Case
import Storage.Queries.Location as Loc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as Products
import System.Environment
import Types.API.ProductInstance

list :: SR.RegistrationToken -> ProdInstReq -> FlowHandler ProductInstanceList
list SR.RegistrationToken {..} ProdInstReq {..} = withFlowHandler $ do
  let personId = PersonId _EntityId
  person <- Person.findById personId
  caseProdList <-
    ProductInstance.listAllProductInstanceWithOffset _limit _offset (ProductInstance.ByCustomerId personId) _status
  caseList <- Case.findAllByIds (ProductInstance._caseId <$> caseProdList)
  prodList <- Products.findAllByIds (ProductInstance._productId <$> caseProdList)
  locList <- Loc.findAllByIds ((Case._fromLocationId <$> caseList) <> (Case._toLocationId <$> caseList))
  return $ catMaybes $ joinIds prodList caseList locList <$> caseProdList
  where
    joinIds :: [Product.Products] -> [Case.Case] -> [Loc.Location] -> ProductInstance.ProductInstance -> Maybe ProductInstanceRes
    joinIds prodList caseList locList caseProd =
      find (\x -> ProductInstance._caseId caseProd == Case._id x) caseList
        >>= buildResponse
      where
        buildResponse k = prepare locList caseProd k <$> find (\z -> ProductInstance._productId caseProd == Product._id z) prodList
        prepare locList caseProd cs prod =
          ProductInstanceRes
            { _case = cs,
              _product = prod,
              _productInstance = caseProd,
              _fromLocation = find (\x -> Case._fromLocationId cs == _getLocationId (Loc._id x)) locList,
              _toLocation = find (\x -> Case._toLocationId cs == _getLocationId (Loc._id x)) locList
            }
