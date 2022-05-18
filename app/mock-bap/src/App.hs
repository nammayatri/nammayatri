module App
  ( runService,
  )
where

import API.Handler
import API.Types
import Beckn.Prelude
import Beckn.Utils.App
import Beckn.Utils.Dhall
import Beckn.Utils.Servant.Server (runServer)
import Beckn.Utils.Servant.SignatureAuth
import Environment
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <- buildAppEnv . configModifier =<< readDhallConfigDefault "mock-bap"
  runServer appEnv (Proxy @API) handler middleware identity EmptyContext (const identity) releaseAppEnv $ \flowRt -> do
    modFlowRtWithAuthManagers flowRt appEnv [(appEnv.selfId, appEnv.authEntity.uniqueKeyId)]
  where
    middleware = hashBodyForSignature
