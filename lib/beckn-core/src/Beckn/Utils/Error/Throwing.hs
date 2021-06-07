module Beckn.Utils.Error.Throwing where

import Beckn.Types.Common
import Beckn.Types.Error.API
import Beckn.Types.Error.APIError
import Beckn.Utils.Logging
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET

type IsAPIException e = (IsAPIError e, Exception e)

throwError :: (MonadThrow m, Log m, IsAPIException e) => e -> m b
throwError err = do
  logWarning $ toLogMessageAPIError err
  throwM err

fromMaybeM ::
  (MonadThrow m, Log m, IsAPIException e) => e -> Maybe b -> m b
fromMaybeM err = maybe (throwError err) pure

fromEitherM ::
  (MonadThrow m, Log m, IsAPIException e) => (left -> e) -> Either left b -> m b
fromEitherM toerr = either (throwError . toerr) pure

throwDBError :: (MonadThrow m, Log m) => ET.DBError -> m a
throwDBError (ET.DBError dbErrType msg) = throwError $
  case dbErrType of
    ET.UnexpectedResult -> SQLResultError msg
    ET.SQLError sqlErr -> SQLRequestError (show sqlErr) msg
    _ -> DBUnknownError msg

checkDBError :: (MonadThrow m, Log m) => ET.DBResult a -> m a
checkDBError = either throwDBError pure

thowRedisError :: (MonadThrow m, Log m) => Text -> m a
thowRedisError = throwError . RedisError
