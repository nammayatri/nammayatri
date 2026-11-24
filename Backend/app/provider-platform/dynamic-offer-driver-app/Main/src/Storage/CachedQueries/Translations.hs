{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Translations (findTextByKey, findAllRowsByKeys, findByOpsKey, findTranslatedText, findOptionalTranslatedText, findTranslatedTexts, findTranslatedTextsCached, findTranslatedTextByOpsKey) where

import Control.Applicative ((<|>))
import qualified Crypto.Hash as Hash
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.TranslationKey (TranslatedText, keyName, lookupNames, textReadByKey)
import Domain.Types.TranslationKeys (TranslationKey)
import qualified Domain.Types.Translations as DTranslations
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Yudhishthira.Types as LYT
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))
import qualified Storage.Queries.TranslationsExtra as QTE

-- | The text of a code-owned key. The reader's language wins over English, and the key's own
-- name over its legacy name: own name, then legacy name, in the reader's language; else English
-- under either name. The legacy name is only read when the own name has no row in the reader's
-- language. Each name resolves the city's row, then the shared row
-- (see "Storage.ConfigPilot.Config.Translation").
findTextByKey :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DMOC.MerchantOperatingCity) -> Language -> TranslationKey -> m (Maybe Text)
findTextByKey mbCityId language key = go (lookupNames key) Nothing
  where
    go [] english = pure english
    go (name : rest) english = do
      mbRow <- byName name
      case mbRow of
        Just row | row.language == language -> pure (Just row.message)
        _ -> go rest (english <|> ((.message) <$> mbRow))
    byName = findByOpsKey mbCityId language

-- | 'findTextByKey' as the translatable text of an API field; the given text when the key has no row.
findTranslatedText :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DMOC.MerchantOperatingCity) -> Language -> TranslationKey -> Text -> m TranslatedText
findTranslatedText mbCityId language key fallback = textReadByKey . fromMaybe fallback <$> findTextByKey mbCityId language key

-- | 'findTranslatedText' for a field that may be absent: the key's text, else the given text when there is one.
findOptionalTranslatedText :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DMOC.MerchantOperatingCity) -> Language -> TranslationKey -> Maybe Text -> m (Maybe TranslatedText)
findOptionalTranslatedText mbCityId language key fallback = fmap textReadByKey . (<|> fallback) <$> findTextByKey mbCityId language key

-- | 'findTranslatedText' for code that names many items at once: the keys are read here, each one once,
-- and the result gives a key's text, else the text given at use.
findTranslatedTexts :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DMOC.MerchantOperatingCity) -> Language -> [TranslationKey] -> m (TranslationKey -> Text -> TranslatedText)
findTranslatedTexts mbCityId language keys = do
  texts <- foldM (\seen key -> if any ((== keyName key) . fst) seen then pure seen else (\mbText -> (keyName key, mbText) : seen) <$> findTextByKey mbCityId language key) [] keys
  pure $ \key fallback -> textReadByKey (fromMaybe fallback (join (lookup (keyName key) texts)))

-- | 'findByOpsKey' as the translatable text of an API field; the given text when the key has no row.
findTranslatedTextByOpsKey :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DMOC.MerchantOperatingCity) -> Language -> Text -> Text -> m TranslatedText
findTranslatedTextByOpsKey mbCityId language key fallback = textReadByKey . maybe fallback (.message) <$> findByOpsKey mbCityId language key

-- | Many keys of one owner (a document type, a stage) as the translatable text of API fields:
-- the first function for a field that always has text, the second for an optional one. Each key
-- reads in the reader's language where it has a row, else its English row, a city row before a
-- shared one, else the given text. One cached read per owner and city, in every language, rather
-- than one per key; none when there are no keys. The cache is named under the driver
-- translations' config-pilot prefix, so a save that clears the translations' cache (the dashboard
-- translation editor, NammaTag) clears it on every pod too. A digest of the keys is part of the
-- name: one document type has a form per vehicle category and a fleet-owner form, each with its
-- own fields, and the name stays short enough for that clear to match it.
findTranslatedTextsCached :: (CacheFlow m r, EsqDBFlow m r) => Text -> Id DMOC.MerchantOperatingCity -> Language -> [TranslationKey] -> m (TranslationKey -> Text -> TranslatedText, TranslationKey -> Maybe Text -> Maybe TranslatedText)
findTranslatedTextsCached owner merchantOpCityId language keys = do
  let keysDigest = show $ Hash.hashWith Hash.SHA256 (TE.encodeUtf8 (T.intercalate "," (DL.sort (map keyName keys))))
  rows <- if null keys then pure [] else IM.withInMemCache ["ConfigPilot", show LYT.TranslationDriver, merchantOpCityId.getId, owner, keysDigest] 3600 $ findAllRowsByKeys keys
  let wanted r = r.language `elem` [language, ENGLISH] && (isNothing r.merchantOperatingCityId || r.merchantOperatingCityId == Just merchantOpCityId)
      rank r = (if r.language == language then 0 else 1 :: Int, if isJust r.merchantOperatingCityId then 0 else 1 :: Int)
      best = Map.fromListWith (\a b -> if rank a <= rank b then a else b) [(r.messageKey, r) | r <- rows, wanted r]
      textOf key = (.message) <$> Map.lookup (keyName key) best
  pure (\key fallback -> textReadByKey (fromMaybe fallback (textOf key)), \key fallback -> textReadByKey <$> (textOf key <|> fallback))

-- | Every row stored under the keys' names, whatever its city or language; the caller picks.
findAllRowsByKeys :: (CacheFlow m r, EsqDBFlow m r) => [TranslationKey] -> m [DTranslations.Translations]
findAllRowsByKeys = QTE.findAllByMessageKeys . map keyName

-- | The row of a key that ops typed into a config row (cancellation reasons, call-feedback options,
-- fare sub-items, coin events): the city's row through config pilot, else the shared row in the
-- language, else the shared English row. Text the code owns has a 'TranslationKey' instead.
findByOpsKey :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DMOC.MerchantOperatingCity) -> Language -> Text -> m (Maybe DTranslations.Translations)
findByOpsKey mbCityId language key =
  getConfig
    (TranslationDimensions {merchantOperatingCityId = getId <$> mbCityId, messageKey = key, language = Just language})
    (Just (QTE.findByErrorAndLanguage key language))
