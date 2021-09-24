module App.Server
  ( run,
  )
where

import App.Routes (mockFcmAPI, mockFcmServer)
import App.Types
import Beckn.Types.App
import qualified Beckn.Utils.Servant.Server as BU
import Servant

run :: EnvR AppEnv -> Application
run = BU.run mockFcmAPI mockFcmServer EmptyContext
