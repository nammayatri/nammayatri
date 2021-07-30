module Product.ProductInstance where

import App.Types
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Storage.Queries.Case as Case
import Storage.Queries.Location as Loc
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as Products
import Types.API.ProductInstance
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Location as Loc
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as SPI
import qualified Types.Storage.Products as Products
import Utils.Common (withFlowHandlerAPI)

list :: Id Person.Person -> [SPI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
list personId status csTypes mlimit moffset = withFlowHandlerAPI $ do
  piList <-
    ProductInstance.listAllProductInstanceWithOffset limit offset (SPI.ByCustomerId personId) status csTypes
  caseList <- Case.findAllByIds (SPI.caseId <$> piList)
  prodList <- Products.findAllByIds (SPI.productId <$> piList)
  locList <- Loc.findAllByIds ((Case.fromLocationId <$> caseList) <> (Case.toLocationId <$> caseList))
  return $ catMaybes $ joinIds prodList caseList locList <$> piList
  where
    limit = toInteger $ fromMaybe 50 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    joinIds :: [Products.Products] -> [Case.Case] -> [Loc.Location] -> SPI.ProductInstance -> Maybe ProductInstanceRes
    joinIds prodList caseList locList prodInst =
      find (\x -> SPI.caseId prodInst == Case.id x) caseList
        >>= buildResponse
      where
        buildResponse k = prepare locList prodInst k <$> find (\z -> SPI.productId prodInst == Products.id z) prodList
        prepare locationList prodInstance cs prod =
          ProductInstanceRes
            { _case = cs,
              product = prod,
              productInstance = prodInstance,
              fromLocation = find (\x -> Case.fromLocationId cs == Loc.id x) locationList,
              toLocation = find (\x -> Case.toLocationId cs == Loc.id x) locationList
            }
