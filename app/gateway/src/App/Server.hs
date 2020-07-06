{-# LANGUAGE TypeApplications #-}

module App.Server
  ( run,
  )
where

import App.Routes
import Beckn.Types.App
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant

type ContextEntries = '[R.FlowRuntime]

mkContext :: Env -> Context ContextEntries
mkContext env = runTime env :. EmptyContext

run :: V.Key (HashMap Text Text) -> Env -> Application
run key env =
  serveWithContext gatewayAPI (mkContext env) $
    hoistServerWithContext gatewayAPI (Proxy @ContextEntries) (f env) (gatewayHandler key)
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err ->
          print ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res
