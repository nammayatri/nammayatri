{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.Server where

import Beckn.Types.App (EnvR, FlowServerR, runTime)
import Data.Kind (Type)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant

type ContextEntries r = '[EnvR r]

class HasEnvEntry r (context :: [Type]) | context -> r where
  getEnvEntry :: Context context -> EnvR r

instance {-# OVERLAPPABLE #-} HasEnvEntry r xs => HasEnvEntry r (notIt ': xs) where
  getEnvEntry (_ :. xs) = getEnvEntry xs

instance {-# OVERLAPPING #-} HasEnvEntry r (EnvR r ': xs) where
  getEnvEntry (x :. _) = x

mkContext :: EnvR r -> Context (ContextEntries r)
mkContext env = env :. EmptyContext

run ::
  forall a r.
  HasServer a (ContextEntries r) =>
  Proxy (a :: Type) ->
  FlowServerR r a ->
  EnvR r ->
  Application
run apis server env =
  serveWithContext apis (mkContext env) $
    hoistServerWithContext apis (Proxy @(ContextEntries r)) f server
  where
    f :: ReaderT (EnvR r) (ExceptT ServerError IO) m -> Handler m
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res
