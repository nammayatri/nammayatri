module Beckn.Utils.Error.Throwing
  ( module Beckn.Utils.Error.Throwing,
    IsAPIException,
  )
where

import Beckn.Types.Common hiding (id)
import Beckn.Types.Error.APIError
import Beckn.Utils.Logging
import EulerHS.Prelude

throwError :: (MonadThrow m, Log m, IsAPIException e) => e -> m b
throwError err = do
  logWarning $ toLogMessageAPIError err
  throwM err

fromMaybeM ::
  (MonadThrow m, Log m, IsAPIException e) => e -> Maybe b -> m b
fromMaybeM err = maybe (throwError err) pure

fromEitherM ::
  (MonadThrow m, Log m, IsAPIException e) => (left -> e) -> Either left b -> m b
fromEitherM toerr = fromEitherM' (throwError . toerr)

liftEither ::
  (MonadThrow m, Log m, IsAPIException e) => Either e b -> m b
liftEither = fromEitherM id

fromEitherM' ::
  Applicative m => (l -> m r) -> Either l r -> m r
fromEitherM' f = either f pure
