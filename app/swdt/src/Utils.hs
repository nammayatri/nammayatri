module Utils where

import Beckn.Prelude
import Beckn.Types.Core.Error
import qualified Data.Text as T
import Data.Time

-- | Read formatted time.
-- Here %F means the same as %Y-%m-%d, and %R acts like %H:%M.
-- Example: readUTCTime "2021-12-01 18:00"
readUTCTime :: Text -> Maybe UTCTime
readUTCTime = parseTimeM True defaultTimeLocale "%F %R" . T.unpack

textToError :: Text -> Error
textToError desc =
  Error
    { _type = CORE_ERROR,
      code = "400",
      path = Nothing,
      message = Just desc
    }
