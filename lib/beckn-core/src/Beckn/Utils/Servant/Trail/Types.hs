module Beckn.Utils.Servant.Trail.Types where

import qualified Data.CaseInsensitive as CI
import EulerHS.Prelude
import qualified Network.HTTP.Types as HTTP

data RequestContent = RequestContent
  { -- | URL path, this excludes host and query parameters and includes captures.
    _path :: [Text],
    -- | Name of HTTP method.
    _method :: Text,
    -- | Query parameters.
    _query :: [(Text, Text)],
    -- | Request headers.
    _headers :: [(Text, Text)],
    _body :: Maybe LByteString
  }
  deriving (Show)

-- | Endpoint identifier (path + method).
_endpointId :: RequestContent -> LText
_endpointId RequestContent {..} =
  mconcat (intersperse "/" $ map toLText _path) <> " " <> toLText _method

keyValueToString :: [(Text, Text)] -> LText
keyValueToString =
  let pairToString (name, value) = toLText name <> "=" <> toLText value
   in mconcat . intersperse ";" . map pairToString

-- | All headers put into one string.
_headersString :: RequestContent -> LText
_headersString = keyValueToString . _headers

-- | All query parameters put into one string.
_queryString :: RequestContent -> LText
_queryString = keyValueToString . _query

decodePath :: ByteString -> [Text]
decodePath =
  -- One could say that we can just store the original 'ByteString',
  -- no decoding is needed, but that bytestring contains url-decoded
  -- data and we don't want it to appear in the database.
  HTTP.decodePathSegments

decodeQueryParam :: HTTP.QueryItem -> (Text, Text)
decodeQueryParam =
  bimap decodeUtf8 (maybe "" decodeUtf8)

decodeHeader :: HTTP.Header -> (Text, Text)
decodeHeader =
  bimap (decodeUtf8 . CI.original) decodeUtf8
