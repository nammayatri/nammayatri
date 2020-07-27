{-# LANGUAGE OverloadedLabels #-}

module Product.ProductInstance where

import App.Types
import Beckn.Types.App as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common (withFlowHandler)
import EulerHS.Prelude
import qualified Models.Case as Case
import qualified Models.Product as Products
import qualified Models.ProductInstance as ProductInstance
import Storage.Queries.Location as Loc
import Types.API.ProductInstance

list :: Person.Person -> [SPI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
list person status csTypes mlimit moffset = withFlowHandler $ do
  piList <-
    ProductInstance.listAllProductInstanceWithOffset limit offset (ByCustomerId $ person ^. #_id) status csTypes
  caseList <- Case.findAllByIds (SPI._caseId <$> piList)
  prodList <- Products.findAllByIds (SPI._productId <$> piList)
  locList <- Loc.findAllByIds ((Case._fromLocationId <$> caseList) <> (Case._toLocationId <$> caseList))
  return $ catMaybes $ joinIds prodList caseList locList <$> piList
  where
    limit = toInteger $ fromMaybe 50 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    joinIds :: [Product.Products] -> [Case.Case] -> [Loc.Location] -> SPI.ProductInstance -> Maybe ProductInstanceRes
    joinIds prodList caseList locList prodInst =
      find (\x -> SPI._caseId prodInst == Case._id x) caseList
        >>= buildResponse
      where
        buildResponse k = prepare locList prodInst k <$> find (\z -> SPI._productId prodInst == Product._id z) prodList
        prepare locList prodInst cs prod =
          ProductInstanceRes
            { _case = cs,
              _product = prod,
              _productInstance = prodInst,
              _fromLocation = find (\x -> Case._fromLocationId cs == _getLocationId (Loc._id x)) locList,
              _toLocation = find (\x -> Case._toLocationId cs == _getLocationId (Loc._id x)) locList
            }
