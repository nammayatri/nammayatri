{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App
  ( runService,
  )
where

import API
import qualified Data.HashMap.Strict as HMS
import Environment
import qualified EulerHS.Runtime as R
import Kernel.Prelude
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runServerWithHealthCheck)
import Servant (Context (..))

runService :: (AppCfg -> AppCfg) -> IO ()
runService cfgModifier = do
  appCfg <- cfgModifier <$> readDhallConfigDefault "mock-google" :: IO AppCfg
  appEnv <- buildAppEnv appCfg
  runServerWithHealthCheck appEnv (Proxy @API) handler identity identity EmptyContext releaseAppEnv \flowRt -> do
    pure flowRt {R._httpClientManagers = HMS.singleton "default" (R._defaultHttpClientManager flowRt)}
