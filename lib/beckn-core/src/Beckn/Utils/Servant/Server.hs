{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.Server where

import Beckn.Types.App (EnvR (..), FlowHandlerR, FlowServerR)
import Beckn.Types.Flow
import EulerHS.Prelude
import Servant
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal)

class HasEnvEntry r (context :: [Type]) | context -> r where
  getEnvEntry :: Context context -> EnvR r

instance {-# OVERLAPPABLE #-} HasEnvEntry r xs => HasEnvEntry r (notIt ': xs) where
  getEnvEntry (_ :. xs) = getEnvEntry xs

instance {-# OVERLAPPING #-} HasEnvEntry r (EnvR r ': xs) where
  getEnvEntry (x :. _) = x

run ::
  forall a r ctx.
  ( HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer a (EnvR r ': ctx)
  ) =>
  Proxy (a :: Type) ->
  FlowServerR r a ->
  Context ctx ->
  EnvR r ->
  Application
run apis server ctx env =
  serveWithContext apis (env :. ctx) $
    hoistServerWithContext apis (Proxy @(EnvR r ': ctx)) f server
  where
    f :: FlowHandlerR r m -> Handler m
    f r = do
      eResult <- liftIO . try $ runReaderT r env
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

runFlowRDelayedIO :: EnvR r -> FlowR r b -> DelayedIO b
runFlowRDelayedIO env f =
  liftIO (try . runFlowR (flowRuntime env) (appEnv env) $ f)
    >>= either delayedFailFatal pure
