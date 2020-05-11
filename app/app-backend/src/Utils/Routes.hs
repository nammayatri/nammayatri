module Utils.Routes where

import Beckn.Types.Core.Api
import Beckn.Types.Core.Contact
import Beckn.Types.Core.Context
import Beckn.Types.Core.Provider
import Epass.Utils.Extra
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Types.App

withFlowHandler :: L.Flow a -> FlowHandler a
withFlowHandler flow = do
  (Env flowRt) <- ask
  lift $ ExceptT $ try $ I.runFlow flowRt $ flow

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
      _contact = contact,
      _api = Api "" lt
    }

contact =
  Contact
    { _email = "",
      _mobile = mobile,
      _landline = landLine,
      _ivr = []
    }

mobile =
  Mobile
    { _country_code = "",
      _number = ""
    }

landLine =
  LandLine
    { _country_code = "",
      _std_code = "",
      _number = "",
      _extension = ""
    }
