module API.Confirm where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import Data.Aeson
import Types.App

confirmServer :: BecknReq Value -> MockM AckResponse
confirmServer _ = do
  mockLog INFO "got confirm request"
  pure Ack
