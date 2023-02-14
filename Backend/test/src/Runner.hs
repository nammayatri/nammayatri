 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Runner
  ( withAppSettings,
    withApp,
  )
where

import Control.Concurrent.Async
import Control.Exception (ErrorCall (..), throwIO)
import EulerHS.Prelude
import Network.Wai
import Network.Wai.Handler.Warp

-- This module mirrors functions from warp, allowing to easily
-- spin up a server for testing purposes. The difference is that
-- while warp runs the server on a free port, withAppSettings and
-- withApp allow to specify the port to run the server on.

data Waiter a = Waiter
  { notify :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return
    Waiter
      { notify = putMVar mvar,
        waitFor = readMVar mvar
      }

withAppSettings :: Settings -> Port -> IO Application -> IO a -> IO a
withAppSettings settings port mkApp action = do
  app <- mkApp
  started <- mkWaiter
  let appSettings =
        settings
          & setPort port
          & setBeforeMainLoop (notify started ())
  result <-
    race
      (runSettings appSettings app)
      (waitFor started >> action)
  case result of
    Left () -> throwIO $ ErrorCall "Unexpected: runSettings exited"
    Right x -> return x

withApp :: Port -> IO Application -> IO a -> IO a
withApp = withAppSettings defaultSettings
