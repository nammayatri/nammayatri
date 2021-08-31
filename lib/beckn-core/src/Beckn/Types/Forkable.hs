module Beckn.Types.Forkable where

import EulerHS.Prelude

class Forkable m where
  fork :: Text -> m () -> m ()

safeFork ::
  (Forkable m, MonadCatch m) =>
  (SomeException -> result) ->
  (success -> result) ->
  Text ->
  (result -> m ()) ->
  m success ->
  m ()
safeFork toError toSuccess name doWithResult f = fork name $ try f >>= doWithResult . either toError toSuccess
