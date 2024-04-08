module Services.Backend where

import Prelude
import Api.Types (NearBySearchRequest(..), NearBySearchRequestRes(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Helpers.Commons (getKeyInSharedPrefKeys)
import Presto.Core.Types.API (class RestEndpoint, Header(..), Headers(..), Response, ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, APIResult, callAPI, loadS)
import Types (OverlayData(..))

getHeaders :: String -> Boolean -> Flow OverlayData Headers
getHeaders val isGzipCompressionEnabled = do
  regToken <- loadS "REGISTERATION_TOKEN"
  pure $ Headers
    $ [ Header "Content-Type" "application/json"
      , Header "x-client-version" (getKeyInSharedPrefKeys "VERSION_NAME")
      , Header "x-bundle-version" (getKeyInSharedPrefKeys "BUNDLE_VERSION")
      , Header "session_id" (getKeyInSharedPrefKeys "SESSION_ID")
      , Header "x-device" (getKeyInSharedPrefKeys "DEVICE_DETAILS")
      ]
    <> case regToken of
        Nothing -> []
        Just token -> [ Header "token" token ]
    <> if isGzipCompressionEnabled then
        [ Header "Accept-Encoding" "gzip" ]
      else
        []
          <> if val /= "" then [ Header "x-f-token" val ] else []

-- unwrapResponse :: forall b. APIResult (Response b) -> Either ErrorResponse b
unwrapResponse ::
  forall b c.
  Either ErrorResponse
    { response :: b
    | c
    } ->
  Either ErrorResponse b
unwrapResponse = case _ of
  Right resp -> Right resp.response
  Left err -> Left err

nearBySearchRequest ∷ Flow OverlayData (Either ErrorResponse NearBySearchRequestRes)
nearBySearchRequest = do
  headers <- getHeaders "" false
  resp <- callAPI headers (NearBySearchRequest "")
  pure $ unwrapResponse resp
