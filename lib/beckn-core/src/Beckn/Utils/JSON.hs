module Beckn.Utils.JSON where

import qualified Data.Text as T
import EulerHS.Prelude

replaceUnderscores :: Text -> Text
replaceUnderscores = T.replace "_" "-"

replaceUnderscoresString :: String -> String
replaceUnderscoresString = T.unpack . replaceUnderscores . T.pack
