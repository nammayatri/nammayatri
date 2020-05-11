module Product.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as Storage
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common (withFlowHandler)
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Person as SP
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.CaseProduct as DB
import qualified Storage.Queries.Products as PQ
import System.Environment
import Types.API.CaseProduct

list :: Maybe Text -> CaseProdReq -> FlowHandler CaseProductList
list regToken CaseProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.findRegistrationTokenByToken regToken
  prodList <- PQ.findAllByTypeOrgId _organisationId _type
  caseProdList <- DB.findAllByIds _limit _offset (Product._id <$> prodList)
  caseList <- CQ.findAllByIds (Storage._caseId <$> caseProdList)
  return $ catMaybes $ joinIds prodList caseList <$> caseProdList
  where
    joinIds :: [Product.Products] -> [Case.Case] -> Storage.CaseProduct -> Maybe CaseProductRes
    joinIds prodList caseList caseProd =
      case find (\x -> (Storage._caseId caseProd) == Case._id x) caseList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare caseProd k) <$> find (\z -> (Storage._productId caseProd) == Product._id z) prodList
        prepare caseProd cs prod =
          CaseProductRes
            { _case = cs,
              _product = prod,
              _caseProduct = caseProd
            }
