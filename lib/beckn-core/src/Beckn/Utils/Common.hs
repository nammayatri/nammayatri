{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Common where

import Beckn.Storage.DB.Config
import Beckn.TypeClass.IsAPIError
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack as Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Core.Error (Error (..))
import Beckn.Types.Error
import Beckn.Utils.Logging
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as DBB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Generics.Labels as GL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import Data.Time.Units (TimeUnit, fromMicroseconds)
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (error, id)
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import Network.HTTP.Types (Header, hContentType)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status
import Servant (ServerError (..), err500)
import qualified Servant.Client as S
import Servant.Client.Core.ClientError
import Servant.Client.Core.Response
import qualified Servant.Server.Internal as S

runFlowR :: R.FlowRuntime -> r -> FlowR r a -> IO a
runFlowR flowRt r x = I.runFlow flowRt . runReaderT x $ r

getCurrLocalTime :: L.MonadFlow m => m LocalTime
getCurrLocalTime = L.runIO $ do
  utc' <- getCurrentTime
  timezone <- getTimeZone utc'
  pure $ utcToLocalTime timezone utc'

getCurrTime :: L.MonadFlow m => m UTCTime
getCurrTime = L.runIO getCurrentTime

roundDiffTimeToUnit :: TimeUnit u => NominalDiffTime -> u
roundDiffTimeToUnit = fromMicroseconds . round . (* 1e6)

fromClientError :: ClientError -> Error
fromClientError err =
  Error
    { _type = "INTERNAL-ERROR",
      _code = "",
      _path = Nothing,
      _message = Just message
    }
  where
    message = case err of
      FailureResponse _ resp -> decodeUtf8 $ responseBody resp
      DecodeFailure _ resp -> decodeUtf8 $ responseBody resp
      UnsupportedContentType _ resp -> decodeUtf8 $ responseBody resp
      InvalidContentTypeHeader resp -> decodeUtf8 $ responseBody resp
      ConnectionError exc -> show exc

checkClientError :: (Log m, L.MonadFlow m) => Context -> Either S.ClientError a -> m a
checkClientError context = \case
  Right x -> pure x
  Left cliErr -> do
    let err = fromClientError cliErr
    logError "client call error" $ (err ^. #_message) ?: "Some error"
    L.throwException $ mkErrResponse context err500 err

throwDBError :: (HasCallStack, L.MonadFlow m, Log m) => ET.DBError -> m a
throwDBError err@(ET.DBError dbErrType msg) = do
  logError "DB error: " (show err)
  uncurry throwErrorWithInfo $
    case dbErrType of
      ET.UnexpectedResult -> (SQLResultError, msg)
      ET.SQLError sqlErr -> (SQLRequestError, makeSqlErrMsg sqlErr msg)
      _ -> (DBUnknownError, msg)
  where
    makeSqlErrMsg sqlErr desc = "SQLError: " <> show sqlErr <> " Description: " <> desc

-- | Get rid of database error
checkDBError :: (HasCallStack, L.MonadFlow m, Log m) => ET.DBResult a -> m a
checkDBError =
  either throwDBError pure

-- | Get rid of database error and empty result
checkDBErrorOrEmpty ::
  (HasCallStack, L.MonadFlow m, Log m, IsAPIError b) =>
  ET.DBResult (Maybe a) ->
  b ->
  m a
checkDBErrorOrEmpty dbres domainErrOnEmpty = case dbres of
  Left err -> throwDBError err
  Right maybeRes -> case maybeRes of
    Nothing -> throwError domainErrOnEmpty
    Just x -> pure x

fromMaybeM ::
  (HasCallStack, MonadThrow m, Log m, IsAPIError a) => a -> Maybe b -> m b
fromMaybeM err = maybe logAndThrow pure
  where
    logAndThrow = do
      throwError err

fromMaybeMWithInfo ::
  (HasCallStack, MonadThrow m, Log m, IsAPIError a) => a -> Text -> Maybe b -> m b
fromMaybeMWithInfo err info = maybe logAndThrow pure
  where
    logAndThrow = do
      throwErrorWithInfo err info

buildErrorBodyWithInfo :: (IsAPIError a) => a -> Text -> BSL.ByteString
buildErrorBodyWithInfo err info = A.encode $ buildAPIErrorWithInfo err info

jsonHeader :: (HeaderName, ByteString)
jsonHeader = (hContentType, "application/json;charset=utf-8")

servantJsonError400,
  servantJsonError401,
  servantJsonError404,
  servantJsonError500,
  servantJsonError503 ::
    Text -> Text -> ServerError
servantJsonError400 err msg = S.err400 {errBody = buildErrorBodyWithInfo err msg, errHeaders = [jsonHeader]}
servantJsonError401 err msg = S.err401 {errBody = buildErrorBodyWithInfo err msg, errHeaders = [jsonHeader]}
servantJsonError404 err msg = S.err404 {errBody = buildErrorBodyWithInfo err msg, errHeaders = [jsonHeader]}
servantJsonError500 err msg = S.err500 {errBody = buildErrorBodyWithInfo err msg, errHeaders = [jsonHeader]}
servantJsonError503 err msg = S.err503 {errBody = buildErrorBodyWithInfo err msg, errHeaders = [jsonHeader]}

mkOkResponse :: L.MonadFlow m => Context -> m AckResponse
mkOkResponse context = do
  currTime <- getCurrTime
  let context' = context {_timestamp = currTime}
  return $ AckResponse context' (ack "ACK") Nothing

mkAckResponse :: L.MonadFlow m => Text -> Text -> m AckResponse
mkAckResponse txnId action = mkAckResponse' txnId action "ACK"

mkAckResponse' :: L.MonadFlow m => Text -> Text -> Text -> m AckResponse
mkAckResponse' txnId action status = do
  currTime <- getCurrTime
  return
    AckResponse
      { _context =
          Context
            { _domain = MOBILITY,
              _country = Just "IND",
              _city = Nothing,
              _action = action,
              _core_version = Nothing,
              _domain_version = Nothing,
              _bap_uri = Nothing,
              _bpp_uri = Nothing,
              _transaction_id = txnId,
              _message_id = txnId,
              _timestamp = currTime,
              _ttl = Nothing
            },
        _message =
          ack status,
        _error = Nothing
      }

mkErrResponse :: Context -> ServerError -> Error -> NackResponseError
mkErrResponse context errBase err =
  NackResponseError
    { _context = context,
      _error = err,
      _status = mkStatus (errHTTPCode errBase) (encodeUtf8 $ errReasonPhrase errBase)
    }

compileErrResponse :: NackResponseError -> AckResponse
compileErrResponse NackResponseError {..} =
  AckResponse
    { _context = _context,
      _message = ack "NACK",
      _error = Just _error
    }

withFlowHandler :: FlowR r a -> FlowHandlerR r a
withFlowHandler flow = do
  (EnvR flowRt appEnv) <- ask
  lift . ExceptT . try . runFlowR flowRt appEnv $ flow

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DT.encodeUtf8

encodeToText :: ToJSON a => a -> Text
encodeToText = DT.decodeUtf8 . BSL.toStrict . A.encode

-- strips double quotes from encoded text
encodeToText' :: ToJSON a => a -> Text
encodeToText' s =
  let s' = A.encode s
   in if BSL.length s' < 2
        then DT.decodeUtf8 $ BSL.toStrict s'
        else DT.decodeUtf8 $ BSL.toStrict $ BSL.tail $ BSL.init s'

authenticate ::
  ( HasField "cronAuthKey" r (Maybe CronAuthKey),
    HasLogContext r
  ) =>
  Maybe CronAuthKey ->
  FlowR r ()
authenticate = check handleKey
  where
    handleKey rauth = do
      key <- check pure =<< getField @"cronAuthKey" <$> ask
      check (flip when throw401 . (key /=)) $
        DT.decodeUtf8 <$> (rightToMaybe . DBB.decode . DT.encodeUtf8 =<< T.stripPrefix "Basic " rauth)
    check = maybe throw401
    throw401 :: HasLogContext r => FlowR r a
    throw401 =
      throwError AuthBlocked

throwHttpError :: forall e m a. (HasCallStack, MonadThrow m, Log m, ToJSON e) => ServerError -> e -> m a
throwHttpError err errMsg = do
  let body = A.encode errMsg
  logError "HTTP_ERROR" (decodeUtf8 body)
  throwM err {errBody = body, errHeaders = jsonHeader : errHeaders err}

throwError ::
  (HasCallStack, MonadThrow m, Log m, IsAPIError a) => a -> m b
throwError err = throwHttpError serverErr $ toAPIError err
  where
    serverErr = toServerError $ toStatusCode err

throwErrorWithInfo ::
  (HasCallStack, MonadThrow m, Log m, IsAPIError a) => a -> Text -> m b
throwErrorWithInfo err info = throwHttpError serverErr $ buildAPIErrorWithInfo err info
  where
    serverErr = toServerError $ toStatusCode err

buildAPIErrorWithInfo :: (IsAPIError a) => a -> Text -> APIError
buildAPIErrorWithInfo err info =
  let apiErr = toAPIError err
      defMsg = apiErr ^. #errorMessage
   in apiErr {errorMessage = defMsg <> " " <> info}

throwAuthError :: (HasCallStack, MonadThrow m, Log m, IsAPIError a) => [Header] -> a -> m b
throwAuthError headers = throwHttpError (S.err401 {errHeaders = headers}) . toAPIError

-- | Format time in IST and return it as text
-- Converts and Formats in the format
-- TODO: make a generic function and then pass format
-- and timezone as arguments. Currently adds +5:30
showTimeIst :: UTCTime -> Text
showTimeIst = T.pack . formatTime defaultTimeLocale "%d %b, %I:%M %p" . addUTCTime (60 * 330)

callClient ::
  (ET.JSONEx a, L.MonadFlow m, Log m) =>
  Text ->
  Context ->
  S.BaseUrl ->
  ET.EulerClient a ->
  m a
callClient = callClient' Nothing

-- TODO: the @desc@ argument should become part of monadic context
callClient' ::
  (ET.JSONEx a, L.MonadFlow m, Log m) =>
  Maybe String ->
  Text ->
  Context ->
  S.BaseUrl ->
  ET.EulerClient a ->
  m a
callClient' mbManager desc context baseUrl cli = do
  endTracking <- L.runIO $ Metrics.startTracking (encodeToText' baseUrl) desc
  res <- L.callAPI' mbManager baseUrl cli
  _ <- L.runIO $ endTracking $ getResponseCode res
  case res of
    Left err -> do
      logError "cli" $ "Failure in " <> show desc <> " call to " <> toText (S.showBaseUrl baseUrl) <> ": " <> show err
      L.throwException $ mkErrResponse context err500 (fromClientError err)
    Right x -> pure x
  where
    getResponseCode res =
      case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"

-- | A replacement for 'L.forkFlow' which works in 'FlowR'.
-- It's main use case is to perform an action asynchronously without waiting for
-- result.
--
-- It has several differences comparing to 'L.forkFlow':
-- * Logs errors in case if the action failed;
-- * Expects action to return '()' - this is good, because the opposite means
--   you ignored something important, e.g. an exception returned explicitly;
-- * Do not log the fact of thread creation (was it any useful?)
--
-- NOTE: this function is temporary, use of bare forking is bad and should be
-- removed one day.

-- I know it is looking similar to forkAsync but I found it simpler to
-- be able to use (FlowR r) instead of (FlowR (EnvR r))
fork :: HasLogContext r => Text -> FlowR r () -> FlowR r ()
fork desc f = do
  env <- ask
  lift $ L.forkFlow desc $ handleExc env $ runReaderT f env
  where
    handleExc env a =
      L.runSafeFlow a >>= \case
        Right () -> pass
        Left e -> runReaderT (err e) env
    err e =
      logWarning "Thread" $
        "Thread " <> show desc <> " died with error: " <> show e

runSafeFlow :: (FromJSON a, ToJSON a) => FlowR r a -> FlowR r (Either Text a)
runSafeFlow flow = do
  env <- ask
  lift $ L.runSafeFlow $ runReaderT flow env

addIfPresent :: [a] -> Maybe a -> [a]
addIfPresent xs (Just x) = x : xs
addIfPresent xs _ = xs

isExpired :: L.MonadFlow m => NominalDiffTime -> UTCTime -> m Bool
isExpired nominal time = do
  now <- getCurrTime
  let addedUTCTime = addUTCTime nominal time
  return $ now > addedUTCTime

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

class HasSchemaName m where
  getSchemaName :: m Text

instance HasDbCfg r => HasSchemaName (FlowR r) where
  getSchemaName =
    asks (schemaName <$> getField @"dbCfg")

-- | Get trace flag from ENV var
getTraceFlag :: HasTraceFlag r => FlowR r TraceFlag
getTraceFlag =
  getField @"traceFlag" <$> ask

padLeft :: Int -> Char -> Text -> Text
padLeft n c txt =
  let prefix = replicate (max 0 $ n - length txt) c
   in T.pack prefix <> txt

-- Suits only for non-negative numbers
padNumber :: Integral i => Int -> i -> Text
padNumber n num = padLeft n '0' $ show (fromIntegral num :: Natural)

-- | An alias for type-level pair of name and type.
type (name :: Symbol) ::: (ty :: Type) = '(name, ty)

-- | Version of 'HasField' which complies with both record-dot-preprocessor
-- and @^. #field@ syntax supported by generics-lens.
--
-- Re-evaluate this once we decide on a uniform way to access fields.
type HasFieldSuper name r ty = (HasField name r ty, GL.Field name r r ty ty)

-- | Bulk version of @HasField@.
type family HasFields (r :: Type) (fields :: [(Symbol, Type)]) :: Constraint where
  HasFields r '[] = () :: Constraint
  HasFields r ('(name, ty) ': fields) =
    (HasFieldSuper name r ty, HasFields r fields)

-- | Require monad to be Flow-based and have specified fields in Reader env.
type HasFlowEnv m r fields =
  ( L.MonadFlow m,
    MonadReader r m,
    HasFields r fields,
    HasLogContext r
  )

foldWIndex :: (Integer -> acc -> a -> acc) -> acc -> [a] -> acc
foldWIndex f acc p = snd $ foldl (\(i, acc') c -> (i + 1, f i acc' c)) (0, acc) p

addLogTag :: HasLogContext env => Text -> FlowR env a -> FlowR env a
addLogTag = local . addLogTagToEnv
