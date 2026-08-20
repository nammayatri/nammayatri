{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Assembles the pure engine's @Map SupportedLanguage LanguageStrings@ from the
-- DB-backed @atlas_app.translations@ table, falling back field-by-field to the
-- static compiled tables ('WhatsappBot.I18n.En' etc.) for anything not (yet)
-- seeded in the DB. This is Phase 1 of the DB-editable-copy work: only 6 pilot
-- fields are looked up (everything else always falls back to the static
-- default), so most languages/fields render exactly as before.
--
-- Runs entirely in 'Flow' (DB + cache access) — this module is the ONLY place
-- in the WhatsApp bot stack that resolves copy from the DB. Its output is a
-- plain, pre-resolved value threaded into 'BotConfig'/'TrackerDeps', so the
-- pure engine (`BotEnv m` is `Monad m =>` ONLY, see whatsapp-bot's CLAUDE.md)
-- never gains an effect.
--
-- No extra caching wraps the assembled map itself: 'LanguageStrings' has
-- function-typed fields (@Text -> Text@ substitution closures), so it cannot
-- derive 'Show'/'ToJSON' and can't go through 'Kernel.Storage.InMem.withInMemCache'
-- as a whole value. Each underlying per-key lookup is already cached (in-mem +
-- Redis, 3600s TTL) by 'Storage.CachedQueries.Translations', so re-assembling
-- the map per message/tick is just a handful of cheap in-mem cache reads once
-- warm, not fresh DB hits.
module WhatsappBot.Adapter.Translations
  ( getTranslationsMap,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Types.Id (Id)
import qualified Storage.CachedQueries.Translations as CQTranslations
import WhatsappBot.I18n.En (en)
import WhatsappBot.I18n.Gu (gu)
import WhatsappBot.I18n.Hi (hi)
import WhatsappBot.I18n.Kn (kn)
import WhatsappBot.I18n.Ta (ta)
import WhatsappBot.I18n.Te (te)
import WhatsappBot.I18n.Types (LanguageStrings (..), SupportedLanguage (..), allLanguages)

-- | 'SupportedLanguage' (wire codes, persisted in Redis — do not touch) →
-- @translations@ table's 'Lang.Language'. All six map 1:1.
toKernelLanguage :: SupportedLanguage -> Lang.Language
toKernelLanguage = \case
  En -> Lang.ENGLISH
  Hi -> Lang.HINDI
  Gu -> Lang.GUJARATI
  Kn -> Lang.KANNADA
  Ta -> Lang.TAMIL
  Te -> Lang.TELUGU

staticFor :: SupportedLanguage -> LanguageStrings
staticFor = \case
  En -> en
  Hi -> hi
  Gu -> gu
  Kn -> kn
  Ta -> ta
  Te -> te

-- | Positional @{{0}}@/@{{1}}@ placeholder substitution for DB-sourced templates.
substitute :: Text -> [Text] -> Text
substitute tmpl args = foldl' (\acc (i, a) -> T.replace ("{{" <> show i <> "}}") a acc) tmpl (zip [0 :: Int ..] args)

-- | The shared cached lookup's Level 3 falls back to the GLOBAL ENGLISH row
-- when the requested language has no row at all (city or global) — the right
-- behaviour for its other callers (show something rather than nothing), but
-- wrong here: an unseeded language (Phase 1: Gujarati/Tamil/Telugu) would
-- silently render our seeded English text instead of staying on its own
-- static compiled copy. Guard against it by rejecting a row whose language
-- doesn't match what was asked for — that only ever happens via that Level 3
-- fallback, since Levels 1-2 only match on the exact requested language.
lookupKey :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Text -> Flow (Maybe Text)
lookupKey mocId lang key = do
  let wantLang = toKernelLanguage lang
  mRow <- CQTranslations.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache mocId key wantLang
  pure $ case mRow of
    Just row | row.language == wantLang -> Just row.message
    _ -> Nothing

-- | DB value if present, else the static compiled default — a partially-seeded
-- DB (or an unseeded language, Phase 1: only English/Hindi/Kannada are seeded)
-- never breaks a message, it just doesn't override it yet.
resolveField :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Text -> Text -> Flow Text
resolveField mocId lang key staticDefault = fromMaybe staticDefault <$> lookupKey mocId lang key

-- | Assemble one language's 'LanguageStrings', overriding only the pilot fields;
-- everything else stays the static record.
buildLanguageStrings :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Flow LanguageStrings
buildLanguageStrings mocId lang = do
  let static = staticFor lang
  welcome' <- resolveField mocId lang "wa_bot_welcome" static.welcome
  chooseLanguage' <- resolveField mocId lang "wa_bot_chooseLanguage" static.chooseLanguage
  setupFailedTmpl <- resolveField mocId lang "wa_bot_setupFailed" (static.setupFailed "{{0}}")
  languageUpdatedTmpl <- resolveField mocId lang "wa_bot_languageUpdated" (static.languageUpdated "{{0}}")
  flexiFareRateTmpl <- resolveField mocId lang "wa_bot_flexiFareRate" (static.flexiFareRate "{{0}}" "{{1}}")
  flexiArrivedWithOtp <- resolveField mocId lang "wa_bot_flexiArrived_withOtp" (static.flexiArrived "{{0}}")
  flexiArrivedNoOtp <- resolveField mocId lang "wa_bot_flexiArrived_noOtp" (static.flexiArrived "")
  pure
    static
      { welcome = welcome',
        chooseLanguage = chooseLanguage',
        setupFailed = \e -> substitute setupFailedTmpl [e],
        languageUpdated = \l -> substitute languageUpdatedTmpl [l],
        flexiFareRate = \base perKm -> substitute flexiFareRateTmpl [base, perKm],
        flexiArrived = \otp -> if T.null otp then flexiArrivedNoOtp else substitute flexiArrivedWithOtp [otp]
      }

-- | Assemble all 6 languages' 'LanguageStrings'. Each field resolution above is
-- a (cached) per-key DB lookup; see the module-level note on why this isn't
-- wrapped in a further whole-map cache.
getTranslationsMap :: Id DMOC.MerchantOperatingCity -> Flow (Map.Map SupportedLanguage LanguageStrings)
getTranslationsMap mocId = Map.fromList <$> mapM (\l -> (l,) <$> buildLanguageStrings mocId l) allLanguages
