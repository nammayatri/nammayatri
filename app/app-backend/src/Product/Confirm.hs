module Product.Confirm where

import Beckn.Types.API.Confirm
import Beckn.Types.App
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Service
import Beckn.Utils.Common (withFlowHandler)
import Epass.Utils.Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Types as Gateway
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Products as QProducts
import Types.App
import Utils.Routes

confirm :: Text -> Text -> FlowHandler Ack
confirm caseId productId = withFlowHandler $ do
  lt <- getCurrentTimeUTC
  currentCase <- QCase.findById $ CaseId caseId
  product <- QProducts.findById $ ProductsId productId
  transactionId <- L.generateGUID
  context <- buildContext "confirm" transactionId
  let service = Service productId Nothing [] [] Nothing [] Nothing Nothing [] Nothing
  return $ Ack "Confirm" "Confirm invoked to transporter"

on_confirm :: OnConfirmReq -> FlowHandler OnConfirmRes
on_confirm OnConfirmReq {..} = withFlowHandler $ do
  -- update the status in DB
  let ack = Ack "ON_CONFIRM" "On Confirm sent to transporter"
  return $ OnConfirmRes context ack
