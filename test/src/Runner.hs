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
