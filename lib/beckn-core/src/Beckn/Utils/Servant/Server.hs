{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.Server where

import Beckn.Prelude (identity)
import qualified Beckn.Tools.Metrics.Init as Metrics
import Beckn.Types.App (EnvR (..), FlowHandlerR, FlowServerR)
import Beckn.Types.Flow
import Beckn.Types.Time
import Beckn.Utils.App
import qualified Beckn.Utils.FlowLogging as L
import Beckn.Utils.IOLogging (LoggerEnv)
import Beckn.Utils.Logging
import qualified Beckn.Utils.Monitoring.Prometheus.Servant as Metrics
import EulerHS.Prelude
import qualified EulerHS.Runtime as E
import GHC.Records.Extra (HasField)
import Network.Wai.Handler.Warp
  ( Port,
    Settings,
    defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
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

runServer ::
  forall env config (api :: Type) ctx.
  ( HasField "config" env config,
    HasField "graceTerminationPeriod" config Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" config L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" config Port,
    Metrics.SanitizedUrl api,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer api (EnvR env ': ctx)
  ) =>
  env ->
  Proxy api ->
  FlowServerR env api ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (E.FlowRuntime -> IO () -> IO ()) ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runServer appEnv serverAPI serverHandler waiMiddleware waiSettings servantCtx serverStartAction shutdownAction initialize = do
  let port = appEnv.config.port
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname $ appEnv.config.loggerConfig
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appEnv.config.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (shutdownAction appEnv))
          & setPort port
          & waiSettings
  let server = withModifiedEnv $ \modifiedEnv ->
        run serverAPI serverHandler servantCtx modifiedEnv
          & logRequestAndResponse modifiedEnv
          & Metrics.addServantInfo serverAPI
          & waiMiddleware
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <-
      runFlowR flowRt appEnv $
        initialize flowRt <* logInfo ("Runtime created. Starting server at port " <> show port)
    serverStartAction flowRt' $ runSettings settings $ server (EnvR flowRt' appEnv)

type HealthCheckAPI = Get '[JSON] Text

healthCheck :: FlowServerR env HealthCheckAPI
healthCheck = pure "App is UP"

runHealthCheckServerWithService ::
  forall env config ctx.
  ( HasField "config" env config,
    HasField "graceTerminationPeriod" config Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" config L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" config Port,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters
  ) =>
  env ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (E.FlowRuntime -> IO ()) ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runHealthCheckServerWithService appEnv waiMiddleware waiSettings servantCtx service =
  runServer appEnv (Proxy @HealthCheckAPI) healthCheck waiMiddleware waiSettings servantCtx forkServerStartService
  where
    forkServerStartService flowRt startServerAction = do
      void $ forkIO startServerAction
      service flowRt

runServerWithHealthCheck ::
  forall env config (api :: Type) ctx.
  ( HasField "config" env config,
    HasField "graceTerminationPeriod" config Seconds,
    HasField "isShuttingDown" env Shutdown,
    HasField "loggerConfig" config L.LoggerConfig,
    HasField "loggerEnv" env LoggerEnv,
    HasField "port" config Port,
    Metrics.SanitizedUrl api,
    HasContextEntry (ctx .++ '[ErrorFormatters]) ErrorFormatters,
    HasServer api (EnvR env ': ctx)
  ) =>
  env ->
  Proxy api ->
  FlowServerR env api ->
  (Application -> Application) ->
  (Settings -> Settings) ->
  Context ctx ->
  (env -> IO ()) ->
  (E.FlowRuntime -> FlowR env E.FlowRuntime) ->
  IO ()
runServerWithHealthCheck appEnv _ serverHandler waiMiddleware waiSettings servantCtx =
  runServer appEnv (Proxy @(HealthCheckAPI :<|> api)) (healthCheck :<|> serverHandler) waiMiddleware waiSettings servantCtx (const identity)
