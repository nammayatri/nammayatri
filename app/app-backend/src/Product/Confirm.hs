module Product.Confirm where

import Beckn.Types.API.Confirm
import Beckn.Types.App
import Beckn.Types.Core.Ack
import Beckn.Utils.Common (withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Types as Gateway

confirm :: Text -> Text -> FlowHandler Ack
confirm caseId productId = withFlowHandler $ do
  -- call transporter
  return $ Ack "Confirm" "Confirm invoked to transporter"

on_confirm :: OnConfirmReq -> FlowHandler OnConfirmRes
on_confirm OnConfirmReq {..} = withFlowHandler $ do
  -- update the status in DB
  let ack = Ack "ON_CONFIRM" "On Confirm sent to transporter"
  return $ OnConfirmRes context ack
