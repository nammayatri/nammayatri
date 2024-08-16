module Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Text as T
import Kernel.Prelude
import Lib.Yudhishthira.Tools.Utils

convertTags :: [Text] -> A.Value
convertTags input = A.object $ map toObject pairs
  where
    pairs = map (T.splitOn "#") input
    toObject [name, value] = (A.fromText $ T.strip name :: A.Key) A..= fromMaybe A.Null (textToMaybeValue (T.strip value) :: Maybe A.Value)
    toObject [name] = (A.fromText $ T.strip name :: A.Key) A..= A.Null
    toObject xs = do
      let reconstructed = T.intercalate "#" xs
      (A.fromText $ T.strip reconstructed :: A.Key) A..= A.Null
