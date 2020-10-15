module Utils.Routes where

import App.Types
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Utils.Common (getCurrTime)
import EulerHS.Prelude

buildContext :: Text -> Text -> Text -> Flow Context
buildContext action txnId msgId = do
  now <- getCurrTime
  return $
    Context
      { _domain = MOBILITY,
        _country = Just "IND",
        _city = Nothing,
        _action = action,
        _core_version = Just "0.8.0",
        _domain_version = Just "0.8.2",
        _transaction_id = txnId,
        _message_id = msgId,
        _bap_uri = Nothing,
        _bpp_uri = Nothing,
        _timestamp = now,
        _ttl = Nothing
      }
