module Types.API.Sms where

import EulerHS.Prelude
import Servant

type ReadSmsAPI =
  "read"
    :> Capture "mobile_number" Text
    :> Get '[JSON] ReadSmsRes

type ReadSmsRes = [Text]
