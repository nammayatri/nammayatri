module Common.Exceptions where

import Beckn.Prelude
import qualified Control.Monad.Catch as C

data MockException
  = OrderNotFound Text
  | OtherError Text
  deriving (Show)

instance C.Exception MockException

fromMaybeM :: (C.Exception e, C.MonadThrow m) => e -> Maybe a -> m a
fromMaybeM err = maybe (C.throwM err) pure

fromEitherM :: (C.Exception err, C.MonadThrow m) => (e -> err) -> Either e a -> m a
fromEitherM errFunc = either (C.throwM . errFunc) pure
