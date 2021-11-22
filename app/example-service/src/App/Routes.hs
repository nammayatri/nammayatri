module App.Routes
  ( ServerAPI,
    serverAPI,
    serverHandler,
  )
where

import App.Types
import Beckn.Prelude
import Beckn.Types.App (FlowServerR)
import Servant

type ServerAPI = Get '[JSON] Text

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

serverHandler :: FlowServerR AppEnv ServerAPI
serverHandler = pure "Hello"
