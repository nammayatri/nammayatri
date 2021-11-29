module App
  ( runService,
  )
where

import App.Routes (serverAPI, serverHandler)
import App.Types
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerService)
import Beckn.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <- readDhallConfigDefault "parking-bap" <&> configModifier >>= buildAppEnv
  runServerService appEnv serverAPI serverHandler identity identity EmptyContext releaseAppEnv \flowRt -> do
    orgShortId <- askConfig (.selfId)
    modFlowRtWithAuthManagers flowRt appEnv [orgShortId]
