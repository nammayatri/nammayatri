-- | i18n facade — port of @ny-connectors/connectors/src/i18n/index.ts@.
-- Re-exports the string-table types + the six language tables via 't', plus
-- 'detectLanguage' and the language-list helper the chooser menu uses.
module WhatsappBot.I18n
  ( SupportedLanguage (..),
    LanguageStrings (..),
    allLanguages,
    languageCode,
    parseLanguage,
    t,
    detectLanguage,
    LanguageInfo (..),
    getAllLanguages,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.Prelude
import WhatsappBot.I18n.Detect (detectLanguage)
import WhatsappBot.I18n.En (en)
import WhatsappBot.I18n.Types

-- | The string table for a language; unset/unknown -> English (@index.ts:20-22@).
--
-- Takes the per-conversation translations map (built once per session/tick by
-- the rider-app adapter from the DB-backed @translations@ table, falling back
-- field-by-field to the static compiled tables above) instead of switching on
-- the static tables directly, so a DB row can override any field without a
-- redeploy. Still 100% pure: the map is a pre-resolved value, not an effect.
t :: Map.Map SupportedLanguage LanguageStrings -> Maybe SupportedLanguage -> LanguageStrings
t m ml = Map.findWithDefault en (fromMaybe En ml) m

-- | One row of the language chooser (@index.ts:24-32@).
data LanguageInfo = LanguageInfo
  { code :: SupportedLanguage,
    name :: Text,
    nativeName :: Text
  }

-- | All languages with their (native) display names, in @languages@ order
-- (en, hi, gu, kn, ta, te — matches index.ts:11-18). Reads names from the
-- SAME resolved translations map every other message uses (DB-backed in
-- production, so editing @wa_bot_languageName@/@wa_bot_nativeLanguageName@ in
-- the DB now actually changes this menu, unlike before when it always read
-- the static compiled tables regardless of the DB).
getAllLanguages :: Map.Map SupportedLanguage LanguageStrings -> [LanguageInfo]
getAllLanguages translations = [mk c | c <- allLanguages]
  where
    mk c =
      let s = t translations (Just c)
       in LanguageInfo {code = c, name = s.languageName, nativeName = s.nativeLanguageName}
