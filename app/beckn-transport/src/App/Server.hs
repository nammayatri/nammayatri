module App.Server where

import           App.Routes
import           Types.App
import qualified Data.Vault.Lazy      as V
import qualified EulerHS.Interpreters as I
import           EulerHS.Prelude
import qualified EulerHS.Types        as T
import           Network.Wai.Parse
import           Servant
import           Servant.Multipart

run :: V.Key (HashMap Text Text) -> Env -> Application
run key env =
  serve transporterAPIs (transporterServer env key)

transporterServer :: Env -> V.Key (HashMap Text Text) -> Server TransporterAPIs
transporterServer env key = hoistServer transporterAPIs (f env) (transporterServer' key)
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err -> do
          print ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res
