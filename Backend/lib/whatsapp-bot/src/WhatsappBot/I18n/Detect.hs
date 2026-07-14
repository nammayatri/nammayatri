-- | First-message script detection — port of
-- @ny-connectors/connectors/src/i18n/detect.ts@. Script-only: romanized text
-- (e.g. romanized Kannada) falls through to 'Nothing' and the caller defaults to
-- English. Used only to seed a NEW user's language; an explicit @lang:@ choice
-- overrides it afterwards.
module WhatsappBot.I18n.Detect (detectLanguage) where

import qualified Data.Text as T
import Kernel.Prelude
import WhatsappBot.I18n.Types (SupportedLanguage (..))

-- | Unicode script blocks -> language, in the SAME order as detect.ts. The
-- language with the most matched characters wins; on a tie the earlier entry
-- wins (detect.ts replaces @best@ only on a strictly-greater count).
scriptRanges :: [(SupportedLanguage, Char -> Bool)]
scriptRanges =
  [ (Kn, inRange '\x0C80' '\x0CFF'), -- Kannada
    (Hi, inRange '\x0900' '\x097F'), -- Devanagari (Hindi)
    (Ta, inRange '\x0B80' '\x0BFF'), -- Tamil
    (Te, inRange '\x0C00' '\x0C7F'), -- Telugu
    (Gu, inRange '\x0A80' '\x0AFF') -- Gujarati
  ]
  where
    inRange lo hi c = c >= lo && c <= hi

detectLanguage :: Text -> Maybe SupportedLanguage
detectLanguage txt
  | T.null txt = Nothing
  | otherwise =
    case filter ((> 0) . snd) [(lang, T.length (T.filter p txt)) | (lang, p) <- scriptRanges] of
      [] -> Nothing
      (x : xs) -> Just . fst $ foldl' (\best cur -> if snd cur > snd best then cur else best) x xs
