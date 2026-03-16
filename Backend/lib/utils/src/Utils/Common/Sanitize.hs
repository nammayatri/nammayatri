module Utils.Common.Sanitize
  ( sanitizeUrl,
    sanitizeErrorMessage,
    sanitizeShowError,
  )
where

import qualified Data.Text as T
import Kernel.Prelude

-- | Strip query parameters from a URL to prevent credential leakage in logs.
-- Replaces everything after '?' with '?<redacted>'.
sanitizeUrl :: Text -> Text
sanitizeUrl url =
  case T.breakOn "?" url of
    (base, query)
      | T.null query -> base
      | otherwise -> base <> "?<redacted>"

-- | Sanitize an error message by stripping query parameters from any URLs
-- found within the text. Handles both http:// and https:// URLs.
sanitizeErrorMessage :: Text -> Text
sanitizeErrorMessage = sanitizeUrlsInText

-- | Convert a Showable error to a sanitized Text, stripping any embedded
-- URLs' query parameters to prevent credential leakage in logs.
sanitizeShowError :: (Show a) => a -> Text
sanitizeShowError = sanitizeErrorMessage . T.pack . show

-- Internal: find and sanitize all URLs in a text
sanitizeUrlsInText :: Text -> Text
sanitizeUrlsInText txt = go txt
  where
    go remaining =
      let (beforeHttp, rest) = breakOnUrl remaining
       in if T.null rest
            then beforeHttp
            else
              let (urlPart, afterUrl) = extractUrl rest
               in beforeHttp <> sanitizeUrl urlPart <> go afterUrl

    breakOnUrl t =
      let (beforeHttps, afterHttps) = T.breakOn "https://" t
          (beforeHttp, afterHttp) = T.breakOn "http://" t
       in if T.null afterHttps && T.null afterHttp
            then (t, "")
            else
              if T.null afterHttps
                then (beforeHttp, afterHttp)
                else
                  if T.null afterHttp
                    then (beforeHttps, afterHttps)
                    else
                      if T.length beforeHttps <= T.length beforeHttp
                        then (beforeHttps, afterHttps)
                        else (beforeHttp, afterHttp)

    -- Extract a URL (up to a whitespace, quote, or end of string)
    extractUrl t =
      let urlEnd c = c == ' ' || c == '"' || c == '\'' || c == ')' || c == ']' || c == '>' || c == '\n' || c == '\r' || c == '}'
          (url, rest) = T.break urlEnd t
       in (url, rest)
