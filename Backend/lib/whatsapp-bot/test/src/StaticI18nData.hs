{-# LANGUAGE TemplateHaskell #-}

-- | Supplies the golden test suite with 'LanguageStrings' for the 5
-- non-English languages, WITHOUT touching a real database (the pure engine
-- and its tests may never do that — see this package's own CLAUDE.md).
--
-- The data itself is a frozen, one-time export of the real
-- @atlas_app.translations@ DB rows (mechanically copied, never hand-retyped,
-- to avoid transcription mistakes) — see @resources/i18n/non-english.json@
-- and its header for the exact export query. It is intentionally a SNAPSHOT,
-- not a live view: if the DB copy is edited later, this file will not
-- automatically follow — re-run the export query to refresh it.
--
-- Reuses the exact same field <-> key mapping as production
-- ('WhatsappBot.I18n.Build.buildLanguageStringsM'), just pointed at this
-- static table instead of a DB call, so the two paths can never quietly
-- drift apart from each other.
module StaticI18nData (staticNonEnglish) where

import Data.Aeson (eitherDecodeStrict')
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.Map.Strict as Map
import Kernel.Prelude
import WhatsappBot.I18n.Build (buildLanguageStringsM)
import WhatsappBot.I18n.Types (LanguageStrings, SupportedLanguage (..))

data Row = Row
  { key :: Text,
    language :: Text,
    message :: Text
  }
  deriving (Generic, FromJSON)

-- | The DB's @language@ column value for each non-English 'SupportedLanguage'
-- (mirrors 'WhatsappBot.Adapter.Translations.toKernelLanguage' — 'En' isn't
-- in this table at all, it stays a static Haskell module).
dbLanguageCode :: SupportedLanguage -> Text
dbLanguageCode = \case
  En -> "ENGLISH" -- unused: En.hs is not part of this export
  Hi -> "HINDI"
  Gu -> "GUJARATI"
  Kn -> "KANNADA"
  Ta -> "TAMIL"
  Te -> "TELUGU"

nonEnglishJson :: ByteString
nonEnglishJson = $(makeRelativeToProject "test/resources/i18n/non-english.json" >>= embedFile)

rows :: [Row]
rows = case eitherDecodeStrict' nonEnglishJson of
  Left err -> error ("StaticI18nData: failed to decode non-english.json: " <> toText err)
  Right rs -> rs

-- | (language code, key) -> message, built once at module load.
rowsByLangKey :: Map.Map (Text, Text) Text
rowsByLangKey = Map.fromList [((r.language, r.key), r.message) | r <- rows]

-- | Look up one field's text for one language; a missing key fails LOUDLY at
-- test-startup (mirrors production's 'resolveField' — no silent fallback).
fetchFor :: SupportedLanguage -> Text -> Text
fetchFor lang k =
  case Map.lookup (dbLanguageCode lang, k) rowsByLangKey of
    Just msg -> msg
    Nothing -> error ("StaticI18nData: missing key " <> show k <> " for " <> show lang <> " in non-english.json")

buildStatic :: SupportedLanguage -> LanguageStrings
buildStatic lang = runIdentity (buildLanguageStringsM (Identity . fetchFor lang))

-- | The 5 non-English languages' 'LanguageStrings', built from the frozen
-- JSON snapshot. 'En' is deliberately excluded — 'WhatsappBot.I18n.En.en'
-- stays a static Haskell module (the fallback safety net + the 6th test
-- language), untouched by this.
staticNonEnglish :: Map.Map SupportedLanguage LanguageStrings
staticNonEnglish = Map.fromList [(l, buildStatic l) | l <- [Hi, Gu, Kn, Ta, Te]]
