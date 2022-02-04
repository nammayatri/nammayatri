{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Beckn.Mock.App where

import Beckn.Types.Common
import Beckn.Utils.IOLogging
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Unlift
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

instance MonadTime (MockM e) where
  getCurrentTime = liftIO getCurrentTime

instance (HasLog e) => Log (MockM e) where
  logOutput = logOutputImplementation
  withLogTag = withLogTagImplementation

instance (HasLog e) => Forkable (MockM e) where
  fork = mockFork

mockFork :: (HasLog e) => Text -> MockM e a -> MockM e ()
mockFork tag action = void $
  withLogTag tag $
    forkFinally action $ \case
      Left se -> logOutput ERROR $ show se
      Right _ -> pure ()
