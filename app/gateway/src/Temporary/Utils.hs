module Temporary.Utils where

import Beckn.Types.Common
import EulerHS.Prelude hiding (id)
import Types.Beckn.Context

withTransactionIdLogTag :: (HasField "context" b Context, Log m) => b -> m a -> m a
withTransactionIdLogTag req = do
  let context = req.context
      transaction_id_ = transaction_id context
  withLogTag ("txnId-" <> transaction_id_)
