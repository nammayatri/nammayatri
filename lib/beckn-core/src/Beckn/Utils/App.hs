module Beckn.Utils.App where

import Beckn.Utils.Logging
import qualified EulerHS.Language as L
import EulerHS.Prelude
import System.Exit (ExitCode)

handleLeft :: (Show a, Log m, L.MonadFlow m) => ExitCode -> Text -> Either a b -> m b
handleLeft exitCode msg = \case
  Left err -> do
    logError (msg <> show err)
    L.runIO $ exitWith exitCode
  Right res -> return res