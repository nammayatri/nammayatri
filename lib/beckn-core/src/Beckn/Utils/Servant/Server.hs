{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Servant.Server where

import Beckn.Types.App (Env, FlowServer, runTime)
import Data.Kind (Type)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant

type ContextEntries = '[R.FlowRuntime]

mkContext :: Env -> Context ContextEntries
mkContext env = runTime env :. EmptyContext

run :: HasServer a ContextEntries => Proxy (a :: Type) -> FlowServer a -> Env -> Application
run apis server env =
  serveWithContext apis (mkContext env) $
    hoistServerWithContext apis (Proxy @ContextEntries) f server
  where
    f :: ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res
