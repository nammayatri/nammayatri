-- | ClickHouse inserts go through clickhouse-haskell's exec, which sends the
-- query with Data.ByteString.Lazy.Char8.pack: every character is cut to its
-- lowest byte, so non-ASCII text arrives corrupted. Escaping it as JSON
-- \uXXXX keeps JSONEachRow rows intact.
module Processor.ClickhouseText
  ( asciiJson,
  )
where

import qualified Data.Text as T
import Kernel.Prelude
import Numeric (showHex)

-- | Escapes every non-ASCII character of JSON text as \uXXXX (a surrogate
-- pair above U+FFFF). Only valid on JSON, where such characters can only sit
-- inside strings.
asciiJson :: Text -> Text
asciiJson = T.concatMap escapeChar
  where
    escapeChar c
      | code < 0x80 = T.singleton c
      | code < 0x10000 = unit code
      | otherwise = unit (0xD800 + (code - 0x10000) `div` 0x400) <> unit (0xDC00 + (code - 0x10000) `mod` 0x400)
      where
        code = fromEnum c
    unit u = "\\u" <> T.justifyRight 4 '0' (T.pack (showHex u ""))
