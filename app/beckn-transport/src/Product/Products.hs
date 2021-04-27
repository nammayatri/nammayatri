{-# LANGUAGE OverloadedLabels #-}

module Product.Products where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common (withFlowHandlerAPI)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Products as PQ
import qualified Test.RandomStrings as RS
import Types.API.Products

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
      { Product._id = Id pid,
        Product._name = req ^. #_name,
        Product._description = req ^. #_description,
        Product._industry = Case.MOBILITY,
        Product._type = Product.RIDE,
        Product._status = Product.INSTOCK,
        Product._shortId = shortId,
        Product._rating = req ^. #_rating,
        Product._review = req ^. #_review,
        Product._price = fromMaybe 0 (req ^. #_price),
        Product._info = req ^. #_info,
        Product._udf1 = req ^. #_udf1,
        Product._udf2 = req ^. #_udf2,
        Product._udf3 = req ^. #_udf3,
        Product._udf4 = req ^. #_udf4,
        Product._udf5 = req ^. #_udf5,
        Product._createdAt = now,
        Product._updatedAt = now
      }
