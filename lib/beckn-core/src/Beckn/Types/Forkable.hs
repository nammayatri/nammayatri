module Beckn.Types.Forkable where

import EulerHS.Prelude

class Forkable m where
  fork :: Text -> m () -> m ()
