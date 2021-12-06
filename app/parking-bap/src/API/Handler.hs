module API.Handler where

import API.API
import App.Types
import Beckn.Prelude
import Beckn.Utils.Common

handler :: FlowServer API
handler personId = withFlowHandlerAPI do
  log DEBUG $ "Hi, " <> showT personId
