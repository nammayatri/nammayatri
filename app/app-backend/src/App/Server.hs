{-# LANGUAGE TypeApplications #-}

module App.Server where

import App.Routes
import Beckn.Types.App
import qualified Data.Vault.Lazy as V
import qualified EulerHS.Interpreters as I
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import Network.Wai.Parse
import Servant
import Servant.Multipart

type ContextEntries = '[R.FlowRuntime]

mkContext :: Env -> Context ContextEntries
mkContext env = runTime env :. EmptyContext

run :: V.Key (HashMap Text Text) -> Env -> Application
run key env =
  serveWithContext appAPIs (mkContext env) (appServer env key)

appServer :: Env -> V.Key (HashMap Text Text) -> Server AppAPIs
appServer env key =
  hoistServerWithContext appAPIs (Proxy @ContextEntries) (f env) (appServer' key)
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err ->
          print ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res
