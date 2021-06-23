module Beckn.Utils.Forkable where

import Beckn.Types.Forkable
import EulerHS.Prelude

safeFork ::
  (Forkable m, MonadCatch m) =>
  (SomeException -> result) ->
  (success -> result) ->
  Text ->
  (result -> m ()) ->
  m success ->
  m ()
safeFork toError toSuccess name doWithResult f = fork name $ try f >>= doWithResult . either toError toSuccess