{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Servant.Server where

import Beckn.Types.App (EnvR, FlowServerR, runTime)
import Data.Kind (Type)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant

type ContextEntries = '[R.FlowRuntime]

mkContext :: EnvR r -> Context ContextEntries
mkContext env = runTime env :. EmptyContext

run ::
  forall a r.
  HasServer a ContextEntries =>
  Proxy (a :: Type) ->
  FlowServerR r a ->
  EnvR r ->
  Application
run apis server env =
  serveWithContext apis (mkContext env) $
    hoistServerWithContext apis (Proxy @ContextEntries) f server
  where
    f :: ReaderT (EnvR r) (ExceptT ServerError IO) m -> Handler m
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res
