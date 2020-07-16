module Utils.Routes where

import App.Types
import Beckn.Types.App
import Beckn.Types.Core.Api
import Beckn.Types.Core.Contact
import Beckn.Types.Core.Context
import Beckn.Types.Core.Provider
import Beckn.Utils.Extra (getCurrentTimeUTC)
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Types.App

buildContext :: Text -> Text -> Flow Context
buildContext action tId = do
  now <- getCurrentTimeUTC
  return $
    Context
      { _domain = "MOBILITY",
        _action = action,
        _version = Nothing,
        _transaction_id = tId,
        _session_id = Nothing,
        _token = Nothing,
        _timestamp = now,
        _status = Nothing
      }

defaultProvider lt =
  Provider
    { _id = "",
      _name = "",
      _website = "",
      _contact = contact_,
      _api = Api "" lt
    }

contact_ =
  Contact
    { email = Just "",
      mobile = Just mobile_,
      landline = Just landLine_,
      ivr = []
    }

mobile_ =
  Mobile
    { country_code = Just "",
      number = Just ""
    }

landLine_ =
  LandLine
    { country_code = "",
      std_code = "",
      number = "",
      extension = ""
    }
