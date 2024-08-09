module Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Kernel.Prelude hiding (encodeUtf8)

convertTags :: [Text] -> A.Value
convertTags input = A.object $ map toObject pairs
  where
    pairs = map (T.splitOn "#") input
    toObject [name, value] = (A.fromText $ T.strip name :: A.Key) A..= fromMaybe A.Null (textToMaybeValue (T.strip value) :: Maybe A.Value)
    toObject [name] = (A.fromText $ T.strip name :: A.Key) A..= A.Null
    toObject _ = error "Invalid tag format"

decodeText :: Text -> Maybe A.Value
decodeText txt = A.decode (fromStrict . encodeUtf8 $ txt)

-- Function to convert Text to Maybe Value
textToMaybeValue :: Text -> Maybe A.Value
textToMaybeValue txt =
  case decodeText txt of
    Just value -> Just value
    Nothing -> decodeText (T.concat ["\"", txt, "\""])
