module Product.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.CaseProduct as DB
import qualified Beckn.Types.Storage.CaseProduct as Storage
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Products as Product
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.Products as PQ

import           Types.API.CaseProduct
import System.Environment
import Types.App
import Utils.Routes


list :: CaseProdReq -> FlowHandler CaseProductList
list CaseProdReq {..} = withFlowHandler $ do
  prodList <- PQ.findAllByTypeOrgId _organisationId _type
  caseProdList <- DB.findAllByIds _limit _offset (Product._id <$> prodList)
  caseList <- CQ.findAllByIds (Storage._caseId <$> caseProdList)
  return $ catMaybes $ joinIds prodList caseList <$> caseProdList
  where
    joinIds :: [Product.Products] -> [Case.Case] -> Storage.CaseProduct -> Maybe CaseProductRes
    joinIds prodList caseList caseProd =
      case find (\ x -> (Storage._caseId caseProd) ==  Case._id x) caseList of
        Just k -> buildResponse k
        Nothing -> Nothing
        where
          buildResponse k = (prepare caseProd k) <$> find (\z -> ( Storage._productId caseProd) == Product._id z) prodList
          prepare caseProd cs prod = CaseProductRes
            { _case = cs
            , _product = prod
            , _caseProduct = caseProd
            }

