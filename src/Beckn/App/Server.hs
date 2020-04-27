module Beckn.App.Server where

import Beckn.App.Routes
import Beckn.Types.App
import qualified Data.Vault.Lazy as V
import qualified EulerHS.Interpreters as I
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant

type FlowServer = ServerT EPassAPIs (ReaderT Env (ExceptT ServerError IO))

run :: V.Key (HashMap Text Text) -> Env -> Application
run key env = serve epassAPIs (epassServer env key)

epassServer :: Env -> V.Key (HashMap Text Text) -> Server EPassAPIs
epassServer env key = hoistServer epassAPIs (f env) (epassServer' key)
  where
    f :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f env r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err -> do
          print ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

epassServer' :: V.Key (HashMap Text Text) -> FlowServer
epassServer' key =
  (healthCheckApp
  :<|> (initiateLogin
       :<|> login
       )
   :<|> (\h -> createPassApplication h
        :<|> listPassApplication h
        :<|> getPassApplicationById h
        :<|> updatePassApplication h
        )
   :<|> (\h -> createOrganization h
        :<|> getOrganization h
        :<|> listOrganization h
        :<|> updateOrganization h
        )
   :<|> getCustomerInfo
   :<|> (\h -> getPassById h
        :<|> updatePass h
        :<|> listPass h
        )
  )
