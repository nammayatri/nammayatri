module Utils.SimpleLogger where

import Beckn.Types.Common
import Beckn.Utils.Logging
import EulerHS.Prelude
import System.IO (hFlush)

instance Log IO where
  logOutput _ tags msg = do
    putStrLn message
    hFlush stdout
    where
      message =
        case tags of
          [] -> msg
          _ -> tagsToText tags <> " " <> msg

  addLogTag _ action = action
