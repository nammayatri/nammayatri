module Utils.Routes where

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

buildContext :: Text -> Text -> L.Flow Context
buildContext action tId = do
  now <- getCurrentTimeUTC
  return $
    Context
      { domain = "MOBILITY",
        action = action,
        version = Nothing,
        transaction_id = tId,
        message_id = Nothing,
        timestamp = now,
        dummy = "DUMMY"
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
