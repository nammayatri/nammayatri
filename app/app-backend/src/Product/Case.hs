module Product.Case where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Products as Products
import Types.API.Case
import Utils.Common (verifyToken)

status ::
  Maybe RegToken ->
  CaseId ->
  FlowHandler StatusRes
status regToken caseId = withFlowHandler $ do
  verifyToken regToken
  case_ <- Case.findById caseId
  cpr <- CaseProduct.findAllByCaseId (Case._id case_)
  products <- Products.findAllByIds (CaseProduct._productId <$> cpr)
  return $ StatusRes case_ products
