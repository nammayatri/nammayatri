{-# LANGUAGE TypeApplications #-}

module App
  ( runService,
  )
where

import API.Handler
import API.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerService)
import Beckn.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Servant (Context (..))
import Tools.Auth ( verifyPersonAction )

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <-
    readDhallConfigDefault "public-transport-bap"
      <&> configModifier
      >>= buildAppEnv
  runServerService appEnv (Proxy @API) handler middleware identity context releaseAppEnv \flowRt -> do
    modFlowRtWithAuthManagers flowRt appEnv []
  where
    middleware =
      hashBodyForSignature
        >>> supportProxyAuthorization
    context = verifyPersonAction @(FlowR AppEnv) :. EmptyContext
