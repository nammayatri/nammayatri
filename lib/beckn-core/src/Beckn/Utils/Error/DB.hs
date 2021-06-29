module Beckn.Utils.Error.DB where

import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Error.BaseError.APIError
import Beckn.Utils.Error.Throwing
import EulerHS.Prelude
import qualified EulerHS.Types as ET

throwDBError :: (MonadThrow m, Log m) => ET.DBError -> m a
throwDBError (ET.DBError dbErrType msg) = throwError $
  case dbErrType of
    ET.UnexpectedResult -> SQLResultError msg
    ET.SQLError sqlErr -> SQLRequestError (show sqlErr) msg
    _ -> DBUnknownError msg

checkDBError :: (MonadThrow m, Log m) => ET.DBResult a -> m a
checkDBError = either throwDBError pure

checkDBErrorOrEmpty ::
  (MonadThrow m, Log m, IsAPIException b) =>
  ET.DBResult (Maybe a) ->
  b ->
  m a
checkDBErrorOrEmpty dbres domainErrOnEmpty =
  either throwDBError (fromMaybeM domainErrOnEmpty) dbres
