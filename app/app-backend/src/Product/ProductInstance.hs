{-# LANGUAGE OverloadedLabels #-}

module Product.ProductInstance where

import App.Types
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import qualified Beckn.Types.Storage.Products as Product
import EulerHS.Prelude
import qualified Models.Case as Case
import qualified Models.Product as Products
import qualified Models.ProductInstance as ProductInstance
import Storage.Queries.Location as Loc
import Types.API.ProductInstance
import Utils.Common (withFlowHandlerAPI)

list :: Person.Person -> [SPI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
list person status csTypes mlimit moffset = withFlowHandlerAPI $ do
  piList <-
    ProductInstance.listAllProductInstanceWithOffset limit offset (SPI.ByCustomerId $ person ^. #id) status csTypes
  caseList <- Case.findAllByIds (SPI.caseId <$> piList)
  prodList <- Products.findAllByIds (SPI.productId <$> piList)
  locList <- Loc.findAllByIds ((Case.fromLocationId <$> caseList) <> (Case.toLocationId <$> caseList))
  return $ catMaybes $ joinIds prodList caseList locList <$> piList
  where
    limit = toInteger $ fromMaybe 50 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    joinIds :: [Product.Products] -> [Case.Case] -> [Loc.Location] -> SPI.ProductInstance -> Maybe ProductInstanceRes
    joinIds prodList caseList locList prodInst =
      find (\x -> SPI.caseId prodInst == Case.id x) caseList
        >>= buildResponse
      where
        buildResponse k = prepare locList prodInst k <$> find (\z -> SPI.productId prodInst == Product.id z) prodList
        prepare locationList prodInstance cs prod =
          ProductInstanceRes
            { _case = cs,
              product = prod,
              productInstance = prodInstance,
              fromLocation = find (\x -> Case.fromLocationId cs == Loc.id x) locationList,
              toLocation = find (\x -> Case.toLocationId cs == Loc.id x) locationList
            }
