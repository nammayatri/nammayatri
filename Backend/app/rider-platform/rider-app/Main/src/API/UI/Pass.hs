{-# OPTIONS_GHC -Wno-orphans #-}

module API.UI.Pass
  ( API,
    handler,
  )
where

import qualified API.Action.UI.Pass
import Environment
import Storage.Beam.SystemConfigs ()

type API = API.Action.UI.Pass.API

handler :: FlowServer API
handler = API.Action.UI.Pass.handler
