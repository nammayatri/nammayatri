module API.Handler where

import qualified API.Types as API
import Beckn.Prelude
import Environment

handler :: FlowServer API.API
handler = pure "Hello, world!"
