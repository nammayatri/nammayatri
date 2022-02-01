{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Beckn.Mock.App where

import Beckn.Types.Common
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Unlift
import Data.Time.Clock hiding (getCurrentTime)
import Relude
import Servant
import UnliftIO.Concurrent

run :: forall e api. HasServer api '[] => Proxy (api :: Type) -> ServerT api (MockM e) -> e -> Application
run proxyApi server env = serve proxyApi $ hoistServer proxyApi f server
  where
    f :: MockM e a -> Handler a
    f action = do
      eithRes <- liftIO . C.try $ runReaderT (runMockM action) env
      case eithRes of
        Left err ->
          liftIO $ print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

newtype MockM e a = MockM {runMockM :: ReaderT e IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader e, MonadIO, MonadUnliftIO, C.MonadThrow, C.MonadCatch, C.MonadMask)

renderLogString :: UTCTime -> LogLevel -> Text -> Text
renderLogString time level str = mconcat [show time, " ", show level, "> ", str]

instance MonadTime (MockM e) where
  getCurrentTime = liftIO getCurrentTime

instance Log (MockM e) where
  logOutput = mockLog
  withLogTag = const identity

mockLog :: LogLevel -> Text -> MockM e ()
mockLog level str = do
  time <- getCurrentTime
  putTextLn $ renderLogString time level str

instance Forkable (MockM e) where
  fork _ = mockFork

mockFork :: MockM e a -> MockM e ()
mockFork action = void $
  forkFinally action $ \case
    Left se -> mockLog ERROR $ show se
    Right _ -> pure ()
