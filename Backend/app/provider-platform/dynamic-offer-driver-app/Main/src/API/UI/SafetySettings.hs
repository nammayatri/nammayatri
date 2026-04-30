{-# OPTIONS_GHC -Wno-orphans #-}

module API.UI.SafetySettings
  ( API,
    handler,
  )
where

import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common (withFlowHandlerAPI)
import qualified Safety.API.UI.SafetySettings as SafetyRoutes
import qualified Safety.Domain.Action.UI.SafetySettings as SafetySettingsLib
import Servant
import Storage.Beam.Sos ()
import Storage.Beam.SystemConfigs ()
import Tools.Auth

instance SafetySettingsLib.HasSafetySettingsHandle AppEnv Flow where
  getSafetySettingsHandle =
    pure
      SafetySettingsLib.ServiceHandle
        { SafetySettingsLib.getPlatformExtras = \_ -> pure (Nothing, Nothing),
          SafetySettingsLib.handlePlatformUpdate = \_ _ -> pure ()
        }

type API = "driver" :> SafetyRoutes.SafetySettingsAPI TokenAuth

handler :: FlowServer API
handler =
  (withFlowHandlerAPI . SafetySettingsLib.getSafetySettings)
    :<|> (\authTuple req -> withFlowHandlerAPI $ SafetySettingsLib.updateSafetySettings authTuple req)
