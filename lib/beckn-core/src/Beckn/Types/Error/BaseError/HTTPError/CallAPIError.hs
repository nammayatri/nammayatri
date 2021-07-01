module Beckn.Types.Error.BaseError.HTTPError.CallAPIError where

import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Utils.Error.Throwing
import EulerHS.Prelude
import Servant.Client (ClientError (..))

data CallAPIError err = RawError ClientError | HTTPError err

extractApiError ::
  FromResponse err =>
  Either ClientError a ->
  Either (CallAPIError err) a
extractApiError = \case
  Left err@(FailureResponse _ response) ->
    Left $ maybe (RawError err) HTTPError (fromResponse response)
  Left l -> Left (RawError l)
  Right a -> Right a

unwrapEitherCallAPIError ::
  ( MonadThrow m,
    Log m,
    IsHTTPException e
  ) =>
  Maybe Text ->
  BaseUrl ->
  (err -> e) ->
  Either (CallAPIError err) a ->
  m a
unwrapEitherCallAPIError errorCodeMb baseUrl toAPIException = fromEitherM' $ \case
  RawError cliErr -> throwError $ ExternalAPICallError errorCodeMb baseUrl cliErr
  HTTPError err -> throwError (toAPIException err)

unwrapEitherOnlyFromRawError ::
  (MonadThrow m, Log m) =>
  Maybe Text ->
  BaseUrl ->
  Either (CallAPIError err) a ->
  m (Either err a)
unwrapEitherOnlyFromRawError errorCodeMb baseUrl = either left (pure . Right)
  where
    left = \case
      RawError cliErr -> throwError $ ExternalAPICallError errorCodeMb baseUrl cliErr
      HTTPError err -> pure (Left err)
