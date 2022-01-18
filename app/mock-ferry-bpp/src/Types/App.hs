{-# LANGUAGE TypeApplications #-}

module Types.App where

import Beckn.Prelude
import Beckn.Types.Logging
import qualified Control.Monad.Catch as C
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock as Time
import Servant
import Types.Environment
import UnliftIO.Concurrent

run :: HasServer api '[] => Proxy (api :: Type) -> ServerT api MockM -> AppEnv -> Application
run proxyApi server env = serve proxyApi $ hoistServer proxyApi f server
  where
    f :: MockM a -> Handler a
    f action = do
      eithRes <- liftIO . C.try $ runReaderT action env
      case eithRes of
        Left err ->
          liftIO $ print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

type MockM = ReaderT AppEnv IO

getCurrentTime :: MockM UTCTime
getCurrentTime = liftIO Time.getCurrentTime

renderLogString :: UTCTime -> LogLevel -> Text -> Text
renderLogString time level str = mconcat [show time, " ", show level, "> ", str]

mockLog :: LogLevel -> Text -> MockM ()
mockLog level str = do
  time <- getCurrentTime
  liftIO $ TIO.putStrLn $ renderLogString time level str

mockFork :: MockM a -> MockM ()
mockFork action = void $
  forkFinally action $ \case
    Left se -> mockLog ERROR $ show se
    Right _ -> pure ()
