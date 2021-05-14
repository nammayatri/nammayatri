{-# LANGUAGE OverloadedLabels #-}

module Product.Log where

import App.Types
import qualified Beckn.Types.Storage.Organization as Org
import Data.Aeson (encode)
import EulerHS.Prelude
import Types.Beckn.API.Log (LogReq)
import Types.Beckn.Ack (AckResponse (..), Status (..), ack)
import Types.Error
import Utils.Common

log :: Org.Organization -> LogReq -> FlowHandler AckResponse
log _org req = withFlowHandlerAPI $ do
  let context = req ^. #_message . #_context
      messageId = context ^. #_message_id
      transactionId = context ^. #_transaction_id
      action = req ^. #_context . #_action
  unless (action == "log") $ throwError InvalidAction
  logTagDebug "gateway" $
    "transaction_id: " <> transactionId
      <> "message_id: "
      <> messageId
      <> ", log: req: "
      <> decodeUtf8 (encode req)
      <> ", context: "
      <> decodeUtf8 (encode context)
  return $ AckResponse (req ^. #_message . #_context) (ack ACK) Nothing
