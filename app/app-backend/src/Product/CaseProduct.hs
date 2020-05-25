module Product.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Location as Loc
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
import qualified Storage.Queries.CaseProduct as CaseProduct
import Storage.Queries.Location as Loc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as Products
import System.Environment
import Types.API.CaseProduct
import Utils.Common (verifyToken)

list :: Maybe Text -> CaseProdReq -> FlowHandler CaseProductList
list regToken CaseProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- verifyToken regToken
  let personId = PersonId _EntityId
  person <- Person.findById personId
  caseProdList <-
    CaseProduct.listAllCaseProductWithOffset _limit _offset (CaseProduct.ByCustomerId personId) _status
  caseList <- Case.findAllByIds (CaseProduct._caseId <$> caseProdList)
  prodList <- Products.findAllByIds (CaseProduct._productId <$> caseProdList)
  locList <- Loc.findAllByIds ((Case._fromLocationId <$> caseList) <> (Case._toLocationId <$> caseList))
  return $ catMaybes $ joinIds prodList caseList locList <$> caseProdList
  where
    joinIds :: [Product.Products] -> [Case.Case] -> [Loc.Location] -> CaseProduct.CaseProduct -> Maybe CaseProductRes
    joinIds prodList caseList locList caseProd =
      case find (\x -> (CaseProduct._caseId caseProd) == Case._id x) caseList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare locList caseProd k) <$> find (\z -> (CaseProduct._productId caseProd) == Product._id z) prodList
        prepare locList caseProd cs prod =
          CaseProductRes
            { _case = cs,
              _product = prod,
              _caseProduct = caseProd,
              _fromLocation = find (\x -> (Case._fromLocationId cs == _getLocationId (Loc._id x))) locList,
              _toLocation = find (\x -> (Case._toLocationId cs == _getLocationId (Loc._id x))) locList
            }
