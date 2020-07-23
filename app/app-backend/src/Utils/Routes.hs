module Utils.Routes where

import App.Types
import Beckn.Types.Core.Context
import Beckn.Utils.Extra (getCurrentTimeUTC)
import EulerHS.Prelude

buildContext :: Text -> Text -> Flow Context
buildContext action tId = do
  now <- getCurrentTimeUTC
  return $
    Context
      { _domain = "MOBILITY",
        _country = Nothing,
        _city = Nothing,
        _action = action,
        _core_version = Nothing,
        _domain_version = Nothing,
        _request_transaction_id = tId,
        _bap_nw_address = Nothing,
        _bg_nw_address = Nothing,
        _bpp_nw_address = Nothing,
        _token = Nothing,
        _timestamp = now
      }
