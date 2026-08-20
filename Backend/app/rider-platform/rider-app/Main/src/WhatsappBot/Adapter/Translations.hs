{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Assembles the pure engine's @Map SupportedLanguage LanguageStrings@ from the
-- DB-backed @atlas_app.translations@ table. A field, once wired here, is a HARD
-- requirement: a missing row throws 'WhatsappBotTranslationNotFound' rather than
-- silently falling back to static copy — by design, per product decision, so a
-- missing seed row is a loud, caught-immediately data bug, not a silent
-- stale-copy regression.
--
-- The actual "which field needs which key(s)" mapping lives in
-- 'WhatsappBot.I18n.Build.buildLanguageStringsM', shared with the golden test
-- suite's @StaticI18nData@ (which supplies the same shape of fetch function,
-- backed by a frozen JSON snapshot instead of a live DB call — the pure engine
-- and its tests may never touch a real DB). This module supplies the DB-backed
-- fetch action.
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
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (fromMaybeM)
import qualified Storage.CachedQueries.Translations as CQTranslations
import Tools.Error (WhatsappBotTranslationError (WhatsappBotTranslationNotFound))
import WhatsappBot.I18n.Build (buildLanguageStringsM)
import WhatsappBot.I18n.Types (LanguageStrings, SupportedLanguage (..), allLanguages)

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

-- | The shared cached lookup's Level 3 falls back to the GLOBAL ENGLISH row
-- when the requested language has no row at all (city or global) — the right
-- behaviour for its other callers (show something rather than nothing), but
-- wrong here: a language unseeded for a given field would silently resolve to
-- our seeded English text instead of a clean "not found". Guard against it by
-- rejecting a row whose language doesn't match what was asked for — that only
-- ever happens via that Level 3 fallback, since Levels 1-2 only match on the
-- exact requested language.
lookupKey :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Text -> Flow (Maybe Text)
lookupKey mocId lang key = do
  let wantLang = toKernelLanguage lang
  mRow <- CQTranslations.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache mocId key wantLang
  pure $ case mRow of
    Just row | row.language == wantLang -> Just row.message
    _ -> Nothing

-- | Required DB lookup for a wired field — throws 'WhatsappBotTranslationNotFound'
-- if the row is missing, rather than silently falling back to static copy. A
-- field only gets called through here once its migration seeds every language,
-- so a throw here means the seed data is missing/wrong, not "not migrated yet".
resolveField :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Text -> Flow Text
resolveField mocId lang key =
  lookupKey mocId lang key >>= fromMaybeM (WhatsappBotTranslationNotFound key (toKernelLanguage lang))

-- | Assemble one language's 'LanguageStrings' — the field <-> key mapping
-- itself lives in 'WhatsappBot.I18n.Build.buildLanguageStringsM'; this just
-- supplies the DB-backed fetch action.
buildLanguageStrings :: Id DMOC.MerchantOperatingCity -> SupportedLanguage -> Flow LanguageStrings
buildLanguageStrings mocId lang = buildLanguageStringsM (resolveField mocId lang)

-- | Assemble all 6 languages' 'LanguageStrings'. Each field resolution above is
-- a (cached) per-key DB lookup; see the module-level note on why this isn't
-- wrapped in a further whole-map cache.
getTranslationsMap :: Id DMOC.MerchantOperatingCity -> Flow (Map.Map SupportedLanguage LanguageStrings)
getTranslationsMap mocId = Map.fromList <$> mapM (\l -> (l,) <$> buildLanguageStrings mocId l) allLanguages
