module API.Handler where

import qualified API.Types as API
import Environment
import Kernel.Prelude

handler :: FlowServer API.API
handler = pure "Hello, world!"
