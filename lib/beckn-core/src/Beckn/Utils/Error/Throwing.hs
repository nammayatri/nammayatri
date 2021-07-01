module Beckn.Utils.Error.Throwing
  ( module Beckn.Utils.Error.Throwing,
    IsHTTPException,
  )
where

import Beckn.Types.Common hiding (id)
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Utils.Logging
import qualified Data.Text as T
import EulerHS.Prelude

throwError :: (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> m b
throwError exc = do
  let someExc = toException exc
  logWarning $ makeLogSomeException someExc
  logCallStack
  throwM someExc
  where
    logCallStack =
      withLogTag "CallStack" $
        logDebug . T.pack $ prettyCallStack callStack

fromMaybeM ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> Maybe b -> m b
fromMaybeM err = maybe (throwError err) pure

fromEitherM ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => (left -> e) -> Either left b -> m b
fromEitherM toerr = fromEitherM' (throwError . toerr)

liftEither ::
  (HasCallStack, MonadThrow m, Log m, IsBaseException e) => Either e b -> m b
liftEither = fromEitherM id

fromEitherM' ::
  Applicative m => (l -> m r) -> Either l r -> m r
fromEitherM' f = either f pure
