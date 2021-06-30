module Beckn.Utils.Error.Throwing
  ( module Beckn.Utils.Error.Throwing,
    IsAPIException,
  )
where

import Beckn.Types.Common hiding (id)
import Beckn.Types.Error.BaseError.APIError
import Beckn.Utils.Logging
import qualified Data.Text as T
import EulerHS.Prelude

throwError :: (HasCallStack, MonadThrow m, Log m, IsBaseException e) => e -> m b
throwError exc = do
  logWarning properLog
  logCallStack
  throwM someExc
  where
    logCallStack =
      withLogTag "CallStack" $
        logDebug . T.pack $ prettyCallStack callStack
    someExc = toException exc
    properLog
      | Just (APIException err) <- fromException someExc = toLogMessageAPIError err
      | Just (BaseException err) <- fromException someExc = fromMaybe "" $ toMessage err
      | otherwise = show exc

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
