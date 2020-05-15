module Utils.Utils where


import EulerHS.Prelude
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL

encodeTypeToText :: (ToJSON a) => a -> Text
encodeTypeToText = TE.decodeUtf8 . BSL.toStrict . A.encode

decodeMTypeFromText :: (FromJSON a) => Text -> Maybe a
decodeMTypeFromText = A.decode . BSL.fromStrict . TE.encodeUtf8

