{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Servant.Server where

import Beckn.Constants.APIErrorCode
import Beckn.Types.App (EnvR, FlowServerR)
import Beckn.Types.Core.Ack (NackResponseError (_status))
import Beckn.Utils.Common
import qualified Data.Aeson as Aeson
import EulerHS.Prelude
import qualified Network.HTTP.Types as H
import Network.Wai (Response, responseLBS)
import Servant

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
    f :: ReaderT (EnvR r) (ExceptT ServerError IO) m -> Handler m
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err ->
          print @String ("exception thrown: " <> show err) *> throwError err
        Right res -> pure res

serverErrorResponse :: ServerError -> Response
serverErrorResponse ex =
  responseLBS
    (H.Status (errHTTPCode ex) $ encodeUtf8 $ errReasonPhrase ex)
    ((H.hContentType, "application/json") : errHeaders ex)
    $ errBody ex

internalErrorResponse :: Response
internalErrorResponse =
  responseLBS
    H.internalServerError500
    [(H.hContentType, "application/json")]
    (Aeson.encode internalServerErr)

nackErrorResponse :: NackResponseError -> Response
nackErrorResponse ex =
  responseLBS
    (_status ex)
    [(H.hContentType, "application/json")]
    (Aeson.encode $ compileErrResponse ex)

exceptionResponse :: SomeException -> Response
exceptionResponse exception
  | Just ex <- fromException exception = serverErrorResponse ex
  | Just ex <- fromException exception = nackErrorResponse ex
  | otherwise = internalErrorResponse
