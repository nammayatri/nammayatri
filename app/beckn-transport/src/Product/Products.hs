{-# LANGUAGE OverloadedLabels #-}

module Product.Products where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Products as Product
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Products as PQ
import qualified Test.RandomStrings as RS
import Types.API.Products
import Utils.Common (withFlowHandlerAPI)

createProduct :: Text -> CreateProdReq -> FlowHandler ProdRes
createProduct _orgId req = withFlowHandlerAPI $ do
  prod <- mkProduct req
  PQ.create prod
  return prod

mkProduct :: CreateProdReq -> Flow Product.Products
mkProduct req = do
  pid <- L.generateGUID
  now <- getCurrentTime
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return $
    Product.Products
      { Product.id = Id pid,
        Product.name = req ^. #name,
        Product.description = req ^. #description,
        Product.industry = Case.MOBILITY,
        Product._type = Product.RIDE,
        Product.status = Product.INSTOCK,
        Product.shortId = shortId,
        Product.rating = req ^. #rating,
        Product.review = req ^. #review,
        Product.price = fromMaybe 0 (req ^. #price),
        Product.info = req ^. #info,
        Product.udf1 = req ^. #udf1,
        Product.udf2 = req ^. #udf2,
        Product.udf3 = req ^. #udf3,
        Product.udf4 = req ^. #udf4,
        Product.udf5 = req ^. #udf5,
        Product.createdAt = now,
        Product.updatedAt = now
      }
