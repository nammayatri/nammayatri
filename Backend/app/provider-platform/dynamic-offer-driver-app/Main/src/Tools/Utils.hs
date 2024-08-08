module Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Text as T
import Kernel.Prelude

convertTags :: [Text] -> A.Value
convertTags input = A.object $ map toObject pairs
  where
    pairs = map (T.splitOn "#") input
    toObject [name, value] = (A.fromText $ T.strip name :: A.Key) A..= T.strip value
    toObject _ = error "Invalid tag format"
