module Utils.Routes where

import App.Types
import Beckn.Types.Core.Context
import Beckn.Utils.Common (getCurrTime')
import EulerHS.Prelude

buildContext :: Text -> Text -> Flow Context
buildContext action tId = do
  now <- getCurrTime'
  return $
    Context
      { _domain = "MOBILITY",
        _country = Nothing,
        _city = Nothing,
        _action = action,
        _core_version = Nothing,
        _domain_version = Nothing,
        _request_transaction_id = tId,
        _bap_id = Nothing,
        _bg_id = Nothing,
        _bpp_id = Nothing,
        _bap_nw_address = Nothing,
        _bg_nw_address = Nothing,
        _bpp_nw_address = Nothing,
        _token = Nothing,
        _timestamp = now
      }
