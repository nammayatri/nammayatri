{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Log where

import App.Types
import Beckn.Types.Core.API.Log (LogReq)
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.Core.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (withFlowHandler)
import Data.Aeson (encode)
import qualified EulerHS.Language as L
import EulerHS.Prelude

log :: Org.Organization -> LogReq -> FlowHandler AckResponse
log _org req = withFlowHandler $ do
  let context = req ^. #_message . #_context
      messageId = context ^. #_message_id
      transactionId = context ^. #_transaction_id
      action = req ^. #_context . #_action
  if action == "log"
    then do
      L.logDebug @Text "gateway" $
        "transaction_id: " <> transactionId
          <> "message_id: "
          <> messageId
          <> ", log: req: "
          <> decodeUtf8 (encode req)
          <> ", context: "
          <> decodeUtf8 (encode context)
      return $ AckResponse (req ^. #_message . #_context) (ack "ACK") Nothing
    else do
      let err = Just $ Error "CONTEXT-ERROR" "" Nothing $ Just $ "Invalid action: " <> action
      return $ AckResponse (req ^. #_message . #_context) (ack "NACK") err
