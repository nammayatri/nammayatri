module Product.Products where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Products as PQ
import qualified Test.RandomStrings as RS
import Types.API.Products
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Products as Product
import Utils.Common (withFlowHandlerAPI)

createProduct :: Id Org.Organization -> CreateProdReq -> FlowHandler ProdRes
createProduct _orgId req = withFlowHandlerAPI $ do
  prod <- mkProduct req
  PQ.create prod
  return prod

mkProduct :: DBFlow m r => CreateProdReq -> m Product.Products
mkProduct req = do
  pid <- L.generateGUID
  now <- getCurrentTime
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return $
    Product.Products
      { Product.id = Id pid,
        Product.name = req.name,
        Product.description = req.description,
        Product.industry = Case.MOBILITY,
        Product._type = Product.RIDE,
        Product.status = Product.INSTOCK,
        Product.shortId = ShortId shortId,
        Product.rating = req.rating,
        Product.review = req.review,
        Product.price = fromMaybe 0 (req.price),
        Product.info = req.info,
        Product.udf1 = req.udf1,
        Product.udf2 = req.udf2,
        Product.udf3 = req.udf3,
        Product.udf4 = req.udf4,
        Product.udf5 = req.udf5,
        Product.createdAt = now,
        Product.updatedAt = now
      }
