module Product.Info where

import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Service
import qualified Beckn.Types.Storage.CaseProduct as SCP
import qualified Beckn.Types.Storage.Products as SProducts
import Beckn.Utils.Common (withFlowHandler)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as DTE
import Epass.Utils.Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Flow as External
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.CaseProduct as QCP
import qualified Storage.Queries.Products as QProducts
import Servant
import Types.App
import Types.API.Location
import Utils.Common (verifyToken)
import Utils.Routes

getProductInfo :: Maybe RegToken -> Text -> FlowHandler Text
getProductInfo regToken prodId = withFlowHandler $ do
  reg <- verifyToken regToken
  caseProduct <- QCP.findByProductId (ProductsId prodId)
  case' <- QCase.findById (SCP._caseId caseProduct)
  product <- QProducts.findById (ProductsId prodId)
  case SProducts._info product of
    Just info -> do
      let decodableValue v = BSL.fromStrict $ DTE.encodeUtf8 v
      case eitherDecode (decodableValue $ fromJust $ SProducts._info product) of
        Right x ->
          let driver = SProducts.driverInfo x
              vehicle = SProducts.vehicleInfo x
          in return ""
        Left err -> L.throwException $ err500 { errBody = encode err }
    Nothing ->
      L.logInfo "get Product info" "No info found in products table" >>
        L.throwException (err400 {errBody = "NO_DETAILS_FOUND"})


getLocation :: Maybe RegToken -> Text -> FlowHandler GetLocationRes
getLocation regToken caseId = withFlowHandler $ do
  verifyToken regToken
  baseUrl <- External.getBaseUrl
  -- TODO: caseId might have to replace with transporter caseId
  resp <- External.location baseUrl caseId
  case resp of
    Left err -> L.throwException $ err500 {errBody = encode err}
    Right r -> return r
