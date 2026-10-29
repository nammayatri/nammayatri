{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The dashboard's view of the translation keys: every domain, its keys, and the text each key
-- has per language in the selected city. Each domain reads and writes its own table.
module Domain.Action.Dashboard.Management.Translation
  ( getTranslationDomains,
    getTranslationKeys,
  )
where

import qualified API.Types.ProviderPlatform.Management.Translation as Common
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPushNotification as DMPN
import Domain.Types.TranslationKey
import Domain.Types.TranslationKeys
import qualified Domain.Types.Translations as DT
import Domain.Types.Trip (generateTripCategoryShowInstances)
import Environment (Flow)
import qualified Environment as Fields
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (Language (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CQMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CQMPN
import qualified Storage.Queries.TranslationsExtra as QTE

-- | One stored text, attributed to the key it belongs to.
data Row = Row
  { rowKey :: Text,
    -- | The (dimension, value) pairs the text is narrowed to, sorted; empty for the text the
    -- sender falls back to.
    rowVariant :: [(Text, Text)],
    rowLanguage :: Language,
    rowField :: Text,
    rowText :: Text,
    rowSource :: Common.TranslationTextSource
  }

-- | A key and the legacy name its rows may still be stored under.
data KeyRef = KeyRef {refKey :: Text, refLegacy :: Maybe Text}

data Domain = Domain
  { domainName :: Text,
    domainFields :: [Text],
    -- | The columns that can narrow one key's text to a variant, with every value the column takes.
    domainDimensions :: [Common.TranslationDimension],
    domainKeys :: [KeyRef],
    readRows :: Id DMOC.MerchantOperatingCity -> Flow [Row]
  }

messageField, titleField, bodyField :: Text
messageField = "message"
titleField = "title"
bodyField = "body"

-- The order the dashboard lists them in.
domains :: [Domain]
domains =
  map translationsDomain ["Document", "FleetOwnerDocument", "OnboardingStage", "Verification", "ServiceTier"]
    <> [pushDomain, smsDomain]

findDomain :: Text -> Flow Domain
findDomain name = find ((== name) . (.domainName)) domains & fromMaybeM (InvalidRequest $ "Unknown translation domain: " <> name)

keyRef :: IsTranslationKey k => k -> KeyRef
keyRef k = KeyRef {refKey = keyName k, refLegacy = legacyKeyName k}

keyDomain :: Text -> Text
keyDomain = T.takeWhile (/= '.')

-- | Picks the stored row that wins for one (key, variant, language, field): the city's own, then
-- the shared one, then one still under the legacy name.
bestRows :: [Row] -> Map.Map (Text, [(Text, Text)], Language, Text) Row
bestRows = Map.fromListWith better . map (\r -> ((r.rowKey, r.rowVariant, r.rowLanguage, r.rowField), r))
  where
    better a b = if rank a <= rank b then a else b
    rank r = case r.rowSource of
      Common.City -> 0 :: Int
      Common.Shared -> 1
      Common.Legacy -> 2

getTranslationDomains :: ShortId DM.Merchant -> Context.City -> Flow Common.TranslationDomainsResp
getTranslationDomains merchantShortId opCity = do
  merchantOpCity <- findCity merchantShortId opCity
  summaries <- forM domains $ \domain -> do
    rows <- bestRows <$> domain.readRows merchantOpCity.id
    -- A text is one field of one key and variant with a row in some language; the sender reads
    -- only the variant that matches, so a variant that exists in one language only is a gap in
    -- the others. Keys with no row in any language are not texts yet.
    let texts = Set.fromList [(key, variant) | (key, variant, _, _) <- Map.keys rows]
        counts = Map.fromListWith (+) [(row.rowLanguage, 1) | row <- Map.elems rows]
    pure
      Common.TranslationDomainSummary
        { domain = domain.domainName,
          fields = domain.domainFields,
          dimensions = domain.domainDimensions,
          keyCount = length domain.domainKeys,
          textCount = Set.size texts * length domain.domainFields,
          filledTexts = [Common.TranslationLanguageCount {language, count} | (language, count) <- Map.toList counts]
        }
  pure Common.TranslationDomainsResp {domains = summaries}

getTranslationKeys :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.TranslationKeysResp
getTranslationKeys merchantShortId opCity domainName = do
  merchantOpCity <- findCity merchantShortId opCity
  domain <- findDomain domainName
  rows <- bestRows <$> domain.readRows merchantOpCity.id
  let entry ref =
        Common.TranslationKeyEntry
          { key = ref.refKey,
            legacyKey = ref.refLegacy,
            texts =
              [ Common.TranslationText {language = r.rowLanguage, field = r.rowField, variant = r.rowVariant, text = r.rowText, source = r.rowSource}
                | r <- Map.elems rows,
                  r.rowKey == ref.refKey
              ]
          }
  pure
    Common.TranslationKeysResp
      { domain = domain.domainName,
        fields = domain.domainFields,
        dimensions = domain.domainDimensions,
        entries = map entry domain.domainKeys
      }

findCity :: ShortId DM.Merchant -> Context.City -> Flow DMOC.MerchantOperatingCity
findCity merchantShortId opCity = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

-- ---------------------------------------------------------------------------
-- translations table: the city's row, then the shared row, then the legacy name
-- ---------------------------------------------------------------------------

translationsDomain :: Text -> Domain
translationsDomain name =
  Domain
    { domainName = name,
      domainFields = [messageField],
      domainDimensions = [],
      domainKeys = refs,
      readRows = readTranslations refs
    }
  where
    refs = [keyRef k | k <- allKeys @TranslationKey, keyDomain (keyName k) == name]

readTranslations :: [KeyRef] -> Id DMOC.MerchantOperatingCity -> Flow [Row]
readTranslations refs cityId = do
  rows <- QTE.findAllByMessageKeys (concatMap (\r -> r.refKey : maybeToList r.refLegacy) refs)
  pure $ mapMaybe toRow rows
  where
    byNew = Map.fromList [(r.refKey, r.refKey) | r <- refs]
    byLegacy = Map.fromList [(legacy, r.refKey) | r <- refs, Just legacy <- [r.refLegacy]]
    toRow t
      | Just key <- Map.lookup t.messageKey byNew,
        Just source <- cityScope t =
        Just (Row key [] t.language messageField t.message source)
      | Just key <- Map.lookup t.messageKey byLegacy,
        Just _ <- cityScope t =
        Just (Row key [] t.language messageField t.message Common.Legacy)
      | otherwise = Nothing
    cityScope t = case t.merchantOperatingCityId of
      Nothing -> Just Common.Shared
      Just c | c == cityId -> Just Common.City
      Just _ -> Nothing

-- ---------------------------------------------------------------------------
-- merchant_push_notification: a row per (key, language, trip category, sub-category)
-- ---------------------------------------------------------------------------

pushDomain :: Domain
pushDomain =
  Domain
    { domainName = "DriverPush",
      domainFields = [titleField, bodyField],
      domainDimensions = pushDimensions,
      domainKeys = map keyRef (allKeys @DriverPushKey),
      readRows = readPushes
    }

tripCategoryDim, fcmSubCategoryDim :: Text
tripCategoryDim = "tripCategory"
fcmSubCategoryDim = "fcmSubCategory"

-- | The values in the text form the columns store ('show').
pushDimensions :: [Common.TranslationDimension]
pushDimensions =
  [ Common.TranslationDimension {name = tripCategoryDim, values = map T.pack generateTripCategoryShowInstances},
    Common.TranslationDimension {name = fcmSubCategoryDim, values = map show [Notification.ByUser, Notification.ByMerchant, Notification.ByDriver, Notification.ByAllocator, Notification.ByApplication]}
  ]

pushVariant :: DMPN.MerchantPushNotification -> [(Text, Text)]
pushVariant pn = List.sort $ catMaybes [(tripCategoryDim,) . show <$> pn.tripCategory, (fcmSubCategoryDim,) . show <$> pn.fcmSubCategory]

pushRows :: DMPN.MerchantPushNotification -> Text -> Common.TranslationTextSource -> [Row]
pushRows pn key source =
  [ Row key (pushVariant pn) pn.language titleField pn.title source,
    Row key (pushVariant pn) pn.language bodyField pn.body source
  ]

readPushes :: Id DMOC.MerchantOperatingCity -> Flow [Row]
readPushes cityId = do
  pns <- CQMPN.findAllByMerchantOpCityId cityId Nothing
  pure $ concatMap toRows pns
  where
    byNew = Map.fromList [(keyName k, keyName k) | k <- allKeys @DriverPushKey]
    byLegacy = Map.fromList [(legacy, keyName k) | k <- allKeys @DriverPushKey, Just legacy <- [legacyKeyName k]]
    toRows pn
      | Just key <- Map.lookup pn.key byNew = pushRows pn key Common.City
      | Just key <- Map.lookup pn.key byLegacy = pushRows pn key Common.Legacy
      | otherwise = []

-- ---------------------------------------------------------------------------
-- merchant_message: the key column stays the MessageKey enum; a row per language
-- ---------------------------------------------------------------------------

smsDomain :: Domain
smsDomain =
  Domain
    { domainName = "Sms",
      domainFields = [messageField],
      domainDimensions = [],
      domainKeys = map keyRef (allKeys @SmsKey),
      readRows = readSms
    }

readSms :: Id DMOC.MerchantOperatingCity -> Flow [Row]
readSms cityId = do
  rows <- CQMM.findAllByMerchantOpCityId cityId Nothing
  pure [Row (keyName (Sms_Body mm.messageKey)) [] mm.language messageField mm.message Common.City | mm <- rows]
