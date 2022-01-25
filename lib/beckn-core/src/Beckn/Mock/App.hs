{-# LANGUAGE TypeApplications #-}

module Beckn.Mock.App where

import Beckn.Types.Logging
import qualified Control.Monad.Catch as C
import qualified Data.Text.IO as TIO
import Data.Time.Clock hiding (getCurrentTime)
import qualified Data.Time.Clock as Time
import Relude
import Servant
import UnliftIO.Concurrent

run :: forall e api. HasServer api '[] => Proxy (api :: Type) -> ServerT api (MockM e) -> e -> Application
run proxyApi server env = serve proxyApi $ hoistServer proxyApi f server
  where
    f :: MockM e a -> Handler a
    f action = do
      eithRes <- liftIO . C.try $ runReaderT action env
      case eithRes of
        Left err ->
          liftIO $ print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

type MockM e = ReaderT e IO

getCurrentTime :: MockM e UTCTime
getCurrentTime = liftIO Time.getCurrentTime

renderLogString :: UTCTime -> LogLevel -> Text -> Text
renderLogString time level str = mconcat [show time, " ", show level, "> ", str]

mockLog :: LogLevel -> Text -> MockM e ()
mockLog level str = do
  time <- getCurrentTime
  liftIO $ TIO.putStrLn $ renderLogString time level str

mockFork :: MockM e a -> MockM e ()
mockFork action = void $
  forkFinally action $ \case
    Left se -> mockLog ERROR $ show se
    Right _ -> pure ()
