module App
  ( runService,
  )
where

import API.API
import API.Handler
import App.Types
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerService)
import Beckn.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Servant (Context (..))
import Tools.Auth

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appEnv <- readDhallConfigDefault "parking-bap" <&> configModifier >>= buildAppEnv
  runServerService appEnv (Proxy @API) handler identity identity context releaseAppEnv \flowRt -> do
    orgShortId <- askConfig (.selfId)
    modFlowRtWithAuthManagers flowRt appEnv [orgShortId]
  where
    context = verifyPersonAction @(FlowR AppEnv) :. EmptyContext
