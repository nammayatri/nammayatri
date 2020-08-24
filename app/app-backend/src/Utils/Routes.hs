module Utils.Routes where

import App.Types
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Utils.Common (getCurrTime)
import EulerHS.Prelude

buildContext :: Text -> Text -> Flow Context
buildContext action tId = do
  now <- getCurrTime
  return $
    Context
      { _domain = MOBILITY,
        _country = Nothing,
        _city = Nothing,
        _action = action,
        _core_version = Nothing,
        _domain_version = Nothing,
        _transaction_id = tId,
        _message_id = tId,
        _bap_uri = Nothing,
        _bpp_uri = Nothing,
        _timestamp = now
      }
