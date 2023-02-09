module App
  ( runSearchResultAggregator,
  )
where

import Environment
import Kernel.Exit
import Kernel.Prelude
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runHealthCheckServerWithService)
import Servant
import qualified Service.Runner as Runner

runSearchResultAggregator :: (AppCfg -> AppCfg) -> IO ()
runSearchResultAggregator configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "search-result-aggregator"
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "

  runHealthCheckServerWithService appEnv identity identity EmptyContext (runService appEnv) releaseAppEnv pure
  where
    runService appEnv flowRt =
      runFlowR flowRt appEnv Runner.run
