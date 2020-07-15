module Product.Info where

import App.Types
import qualified Beckn.Types.API.Track as Tracker
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Service
import qualified Beckn.Types.Mobility.Trip as Trip
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import qualified Beckn.Types.Storage.Products as SProducts
import Beckn.Utils.Common (decodeFromText, withFlowHandler)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as DTE
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Flow as External
import Servant
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.Products as QProducts
import Types.API.Location
import Types.API.Product
import Types.App
import Types.ProductInfo as ProductInfo
import Utils.Routes

getProductInfo :: Person.Person -> Text -> FlowHandler GetProductInfoRes
getProductInfo person prodInstId = withFlowHandler $ do
  productInstance <- QPI.findById (ProductInstanceId prodInstId)
  case' <- QCase.findIdByPerson person (SPI._caseId productInstance)
  case decodeFromText =<< SPI._info productInstance of
    Just (info :: ProductInfo) ->
      case ProductInfo._tracker info of
        Nothing -> L.throwException $ err500 {errBody = "NO_TRACKING_INFORMATION_FOUND"}
        Just tracker -> do
          let trip = Tracker.trip tracker
          return $
            GetProductInfoRes
              { vehicle = Trip.vehicle trip,
                driver = Trip.driver trip,
                travellers = Trip.travellers trip,
                fare = Trip.fare trip,
                caseId = _getCaseId (SPI._caseId productInstance),
                product = productInstance
              }
    Nothing ->
      L.logInfo "get Product info" "No info found in products table"
        >> L.throwException (err400 {errBody = "NO_DETAILS_FOUND"})

getLocation :: Person.Person -> Text -> FlowHandler GetLocationRes
getLocation person caseId = withFlowHandler $ do
  baseUrl <- External.getBaseUrl
  productInstances <- QPI.listAllProductInstanceByPerson person (QPI.ByApplicationId $ CaseId caseId) [SPI.CONFIRMED]
  when (null productInstances) $ L.throwException $ err400 {errBody = "INVALID_CASE"}
  -- TODO: what if there are multiple CONFIRMED products possible?
  case decodeFromText =<< SPI._info (head productInstances) of
    Nothing -> L.throwException $ err500 {errBody = "NO_TRACKING_INFORMATION_FOUND"}
    Just (info :: ProductInfo) -> do
      let mtracker = ProductInfo._tracker info
      case mtracker of
        Nothing -> L.throwException $ err500 {errBody = "NO_TRACKING_INFORMATION_FOUND"}
        Just tracker -> do
          resp <- External.location baseUrl (Trip.id $ Tracker.trip tracker)
          case resp of
            Left err -> L.throwException $ err500 {errBody = encode err}
            Right r -> return r
