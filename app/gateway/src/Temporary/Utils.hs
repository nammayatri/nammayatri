{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Temporary.Utils where

import Beckn.Types.Common
import Beckn.Types.Core.Error
import Beckn.Utils.Error.Throwing
import EulerHS.Prelude hiding (id)
import GHC.Records (HasField (..))
import Types.Beckn.Ack as Ack
import Types.Beckn.Context

mkOkResponse :: MonadTime m => Context -> m AckResponse
mkOkResponse context = do
  currTime <- getCurrentTime
  let context' = context {_timestamp = currTime}
  return $ AckResponse context' (ack Ack.ACK) Nothing

withTransactionIdLogTag :: (HasField "context" b Context, Log m) => b -> m a -> m a
withTransactionIdLogTag req = do
  let context = getField @"context" req
      transaction_id = _transaction_id context
  withLogTag ("txnId-" <> transaction_id)

checkAckResponseError :: (MonadThrow m, Log m, IsAPIException e) => (Error -> e) -> AckResponse -> m ()
checkAckResponseError err ackResp = whenJust (ackResp ^. #_error) (throwError . err)
