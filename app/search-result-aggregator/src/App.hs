module App
  ( runSearchResultAggregator,
  )
where

import Beckn.Exit
import Beckn.Prelude
import Beckn.Types.Flow (runFlowR)
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runHealthCheckServerWithService)
import Environment
import Servant
import qualified Service.Runner as Runner

runSearchResultAggregator :: (AppCfg -> AppCfg) -> IO ()
runSearchResultAggregator configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "search_result_aggregator"
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "

  runHealthCheckServerWithService appEnv identity identity EmptyContext (runService appEnv) releaseAppEnv pure
  where
    runService appEnv flowRt =
      runFlowR flowRt appEnv Runner.run
