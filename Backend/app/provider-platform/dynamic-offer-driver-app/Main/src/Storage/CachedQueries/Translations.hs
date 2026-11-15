{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Translations (findTextByKey, findAllRowsByKeys, findByOpsKey) where

import Control.Applicative ((<|>))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.TranslationKey (keyName, lookupNames)
import Domain.Types.TranslationKeys (TranslationKey)
import qualified Domain.Types.Translations as DTranslations
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
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
