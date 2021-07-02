module Beckn.Types.Error.BaseError.HTTPError.CallAPIError where

import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Error.BaseError.HTTPError (IsBaseException)
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Utils.Error.Throwing
import EulerHS.Prelude
import Servant.Client (ClientError (..))

data CallAPIError err = RawError ClientError | APIError err

extractApiError ::
  FromResponse err =>
  Either ClientError a ->
  Either (CallAPIError err) a
extractApiError = \case
  Left err@(FailureResponse _ response) ->
    Left $ maybe (RawError err) APIError (fromResponse response)
  Left l -> Left (RawError l)
  Right a -> Right a

unwrapEitherCallAPIError ::
  ( MonadThrow m,
    Log m,
    IsBaseException e
  ) =>
  Maybe Text ->
  BaseUrl ->
  (err -> e) ->
  Either (CallAPIError err) a ->
  m a
unwrapEitherCallAPIError errorCodeMb baseUrl toBaseException = fromEitherM' $ \case
  RawError cliErr -> throwError $ ExternalAPICallError errorCodeMb baseUrl cliErr
  APIError err -> throwError (toBaseException err)

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
      APIError err -> pure (Left err)
