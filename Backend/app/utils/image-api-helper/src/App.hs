 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App
  ( runMock,
  )
where

import API.Total
import Environment
import Kernel.Mock.App hiding (runMock)
import Kernel.Prelude
import Kernel.Types.Common (defaultLoggerConfig)
import Kernel.Utils.App (logRequestAndResponseGeneric)
import Kernel.Utils.Logging
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )

runMock :: (AppCfg -> AppCfg) -> IO ()
runMock _cfgModifier = do
  let appCfg = defaultAppCfg
  withAppEnv appCfg $ \appEnv -> do
    let port = appCfg.port
        settings =
          defaultSettings & setPort port
        reqRespLogger :: Text -> Text -> IO ()
        reqRespLogger tag info = runReaderT (runMockM $ withLogTag tag $ logOutput INFO info) appEnv

    runSettings settings $
      logRequestAndResponseGeneric reqRespLogger $
        run totalAPI totalServer appEnv

defaultAppCfg :: AppCfg
defaultAppCfg =
  AppCfg
    { port = 8099,
      loggerConfig = defaultLoggerConfig
    }
