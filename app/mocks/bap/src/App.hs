module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Environment
import Kernel.Prelude
import Kernel.Utils.App
import Kernel.Utils.Dhall
import Kernel.Utils.Servant.Server (runServer)
import Kernel.Utils.Servant.SignatureAuth
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <- buildAppEnv . configModifier =<< readDhallConfigDefault "mock-bap"
  runServer appEnv (Proxy @API) handler middleware identity EmptyContext (const identity) releaseAppEnv $ \flowRt -> do
    modFlowRtWithAuthManagers flowRt appEnv [(appEnv.selfId, appEnv.authEntity.uniqueKeyId)]
  where
    middleware = hashBodyForSignature
