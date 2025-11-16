{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.UI.Pass
  ( API,
    handler,
  )
where

import qualified API.Action.UI.Pass
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = API.Action.UI.Pass.API

handler :: FlowServer API
handler = API.Action.UI.Pass.handler
