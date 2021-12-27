module API.Handler where

import qualified API.Types as API
import App.Types
import Beckn.Prelude

handler :: FlowServer API.API
handler = pure "Hello, world!"
