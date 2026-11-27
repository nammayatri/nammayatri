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
module Domain.Action.Dashboard.Translation
  ( getTranslationDomains,
    getTranslationKeys,
    postTranslationUpsert,
  )
where

import qualified API.Types.RiderPlatform.Management.Translation as Common
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
import EulerHS.Prelude hiding (id, readMaybe)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (Language (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import qualified Lib.Payment.Domain.Types.Offer as Fields
import qualified Lib.Payment.Storage.Queries.Offer as QOffer
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CQMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CQMPN
import qualified Storage.CachedQueries.Translations as CQTranslations
import qualified Storage.Queries.MerchantMessage as QMM
import qualified Storage.Queries.MerchantPushNotification as QMPN
import qualified Storage.Queries.Translations as QT
import qualified Storage.Queries.TranslationsExtra as QTE
import Text.Read (readMaybe)
import qualified Tools.Notifications as Notify

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
    domainKeys :: DMOC.MerchantOperatingCity -> Flow [KeyRef],
    readRows :: [KeyRef] -> Id DMOC.MerchantOperatingCity -> Flow [Row],
    -- | Saves the cells of one key, variant and language: one row where the domain stores them together.
    writeEntry :: DMOC.MerchantOperatingCity -> [Common.TranslationUpsertEntry] -> Flow ()
  }

messageField, titleField, bodyField :: Text
messageField = "message"
titleField = "title"
bodyField = "body"

-- The order the dashboard lists them in.
domains :: [Domain]
domains = [translationsDomain "ServiceTier", translationsDomain "Refund", translationsDomain "TripCategory", translationsDomain "LiveActivity", translationsDomain "Frfs", translationsDomain "FareUpdate", offerDomain, pushDomain, smsDomain]

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

-- | An upsert entry's variant in the sorted form rows carry.
entryVariant :: Common.TranslationUpsertEntry -> [(Text, Text)]
entryVariant entry = List.sort (fromMaybe [] entry.variant)

-- | A variant's value for one dimension, parsed back into the column's type.
-- 'postTranslationUpsert' has already checked it against the dimension's values.
variantValue :: Read a => Text -> [(Text, Text)] -> Flow (Maybe a)
variantValue name variant = forM (List.lookup name variant) $ \v -> readMaybe (T.unpack v) & fromMaybeM (InvalidRequest $ "Bad " <> name <> " value: " <> v)

-- | The @{#name#}@ placeholders in a template, sorted.
placeholders :: Text -> [Text]
placeholders t = List.nub $ List.sort [T.takeWhile (/= '#') p | p <- drop 1 (T.splitOn "{#" t), "#}" `T.isInfixOf` p]

getTranslationDomains :: ShortId DM.Merchant -> Context.City -> Flow Common.TranslationDomainsResp
getTranslationDomains merchantShortId opCity = do
  merchantOpCity <- findCity merchantShortId opCity
  summaries <- forM domains $ \domain -> do
    keyRefs <- domain.domainKeys merchantOpCity
    rows <- bestRows <$> domain.readRows keyRefs merchantOpCity.id
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
          keyCount = length keyRefs,
          textCount = Set.size texts * length domain.domainFields,
          filledTexts = [Common.TranslationLanguageCount {language, count} | (language, count) <- Map.toList counts]
        }
  pure Common.TranslationDomainsResp {domains = summaries}

getTranslationKeys :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.TranslationKeysResp
getTranslationKeys merchantShortId opCity domainName = do
  merchantOpCity <- findCity merchantShortId opCity
  domain <- findDomain domainName
  keyRefs <- domain.domainKeys merchantOpCity
  rows <- bestRows <$> domain.readRows keyRefs merchantOpCity.id
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
        entries = map entry keyRefs
      }

postTranslationUpsert :: ShortId DM.Merchant -> Context.City -> Text -> Common.TranslationUpsertReq -> Flow Common.TranslationUpsertRes
postTranslationUpsert merchantShortId opCity domainName req = do
  merchantOpCity <- findCity merchantShortId opCity
  domain <- findDomain domainName
  keyRefs <- domain.domainKeys merchantOpCity
  stored <- bestRows <$> domain.readRows keyRefs merchantOpCity.id
  let check entry = do
        unless (any ((== entry.key) . (.refKey)) keyRefs) $
          throwError $ InvalidRequest $ "Key " <> entry.key <> " is not in domain " <> domainName
        unless (entry.field `elem` domain.domainFields) $
          throwError $ InvalidRequest $ "Field " <> entry.field <> " is not in domain " <> domainName
        let variant = entryVariant entry
        when (List.nub (map fst variant) /= map fst variant) $
          throwError $ InvalidRequest $ "A dimension appears twice in the variant of " <> entry.key
        forM_ variant $ \(name, value) -> do
          dim <- find ((== name) . (.name)) domain.domainDimensions & fromMaybeM (InvalidRequest $ "Dimension " <> name <> " is not in domain " <> domainName)
          unless (value `elem` dim.values) $
            throwError $ InvalidRequest $ "Value " <> value <> " is not allowed for " <> name
        when (T.null (T.strip entry.text)) $
          throwError $ InvalidRequest $ "Empty text for " <> entry.key <> " " <> show entry.language
      -- Every language of a text carries the same placeholders, or the sender's substitution
      -- leaves a hole in some language. A text is one key, field and variant; a variant no other
      -- language has a row of yet is held to the key's general text. Per language the row the
      -- sender reads counts (the city's, then the shared one, then the legacy name's) as it stands
      -- after this request, so one import can change the placeholders of every language together.
      checkPlaceholders afterSave entry = do
        let otherLanguages ofVariant = [r | ((k, v, l, f), r) <- Map.toList afterSave, k == entry.key, v == ofVariant, f == entry.field, l /= entry.language]
            compared = case otherLanguages (entryVariant entry) of
              [] -> otherLanguages []
              rows -> rows
            describeSet lang txt = show lang <> ": " <> (if null (placeholders txt) then "none" else T.intercalate ", " (placeholders txt))
        unless (all ((== placeholders entry.text) . placeholders . (.rowText)) compared) $
          throwError $ InvalidRequest $ "Placeholders of " <> entry.key <> " must be the same in every language; after this save: " <> T.intercalate "; " (describeSet entry.language entry.text : [describeSet r.rowLanguage r.rowText | r <- compared])
      -- The entries of one key, variant and language stand or fall together: a push's title and
      -- body are checked together and written as one row.
      groupOf entry = (entry.key, entryVariant entry, entry.language)
      byGroup = List.groupBy (\a b -> groupOf a == groupOf b)
      failure reason entry = Common.TranslationUpsertFailure {key = entry.key, language = entry.language, field = entry.field, variant = entry.variant, reason}
      attempt trial (okCells, failed) cells =
        withTryCatch "postTranslationUpsert" (trial cells) >>= \case
          Right () -> pure (okCells <> cells, failed)
          Left exc -> pure (okCells, failed <> map (failure (failureReason exc)) cells)
  -- Every cell is tried, so a bad cell holds back its own key only, and the response names each
  -- cell that was not saved. The placeholders are compared once the other checks have passed, with
  -- the cells that passed them standing in for the stored rows.
  (checkedCells, failedChecks) <- foldM (attempt (mapM_ check)) ([], []) (byGroup (List.sortOn groupOf req.entries))
  let afterSave = Map.union (Map.fromList [((e.key, entryVariant e, e.language, e.field), Row e.key (entryVariant e) e.language e.field e.text Common.City) | e <- checkedCells]) stored
  (saved, failedWrites) <- foldM (attempt (\cells -> mapM_ (checkPlaceholders afterSave) cells >> domain.writeEntry merchantOpCity cells)) ([], []) (byGroup checkedCells)
  pure Common.TranslationUpsertRes {saved = length saved, failed = failedChecks <> failedWrites}
  where
    failureReason exc
      | Just (BaseException err) <- fromException exc = fromMaybe (show err) (toMessage err)
      | otherwise = show exc

findCity :: ShortId DM.Merchant -> Context.City -> Flow DMOC.MerchantOperatingCity
findCity merchantShortId opCity =
  CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)

-- ---------------------------------------------------------------------------
-- translations table: the city's row, then the shared row, then the legacy name
-- ---------------------------------------------------------------------------

translationsDomain :: Text -> Domain
translationsDomain name =
  Domain
    { domainName = name,
      domainFields = [messageField],
      domainDimensions = [],
      domainKeys = const (pure refs),
      readRows = readTranslations,
      writeEntry = mapM_ . writeTranslation
    }
  where
    refs = [keyRef k | k <- allKeys @RiderTranslationKey, keyDomain (keyName k) == name]

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

writeTranslation :: DMOC.MerchantOperatingCity -> Common.TranslationUpsertEntry -> Flow ()
writeTranslation merchantOpCity entry = do
  rows <- QTE.findAllByMessageKey entry.key
  now <- getCurrentTime
  case find (\t -> t.merchantOperatingCityId == Just merchantOpCity.id && t.language == entry.language) rows of
    Just t -> QT.updateByPrimaryKey t {DT.message = entry.text}
    Nothing -> do
      id <- generateGUID
      QT.create
        DT.Translations
          { id,
            messageKey = entry.key,
            language = entry.language,
            message = entry.text,
            merchantOperatingCityId = Just merchantOpCity.id,
            createdAt = now,
            updatedAt = now
          }
  invalidateConfigInMem LYT.TranslationRider
  CQTranslations.clearCache merchantOpCity.id entry.key entry.language

-- | One key per text of each of the city's active offers. The texts are @translations@ rows
-- like the fixed keys'; only the key list comes from the offer rows.
offerDomain :: Domain
offerDomain =
  Domain
    { domainName = "Offer",
      domainFields = [messageField],
      domainDimensions = [],
      domainKeys = \city -> do
        offers <- QOffer.findAllActiveByMerchant city.merchantId.getId city.id.getId True
        pure [keyRef (key offer.id.getId) | offer <- offers, key <- [Offer_Title, Offer_Description, Offer_Tnc]],
      readRows = readTranslations,
      writeEntry = mapM_ . writeTranslation
    }

-- ---------------------------------------------------------------------------
-- merchant_push_notification: a row per (key, language, trip category, sub-category)
-- ---------------------------------------------------------------------------

pushDomain :: Domain
pushDomain =
  Domain
    { domainName = "RiderPush",
      domainFields = [titleField, bodyField],
      domainDimensions = pushDimensions,
      domainKeys = const (pure (map keyRef (allKeys @RiderPushKey))),
      readRows = const readPushes,
      writeEntry = writePush
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
    byNew = Map.fromList [(keyName k, keyName k) | k <- allKeys @RiderPushKey]
    byLegacy = Map.fromList [(legacy, keyName k) | k <- allKeys @RiderPushKey, Just legacy <- [legacyKeyName k]]
    toRows pn
      | Just key <- Map.lookup pn.key byNew = pushRows pn key Common.City
      | Just key <- Map.lookup pn.key byLegacy = pushRows pn key Common.Legacy
      | otherwise = []

writePush :: DMOC.MerchantOperatingCity -> [Common.TranslationUpsertEntry] -> Flow ()
writePush _ [] = pure ()
writePush merchantOpCity cells@(entry : _) = do
  key :: RiderPushKey <- parseKey entry.key & fromMaybeM (InvalidRequest $ "Unknown push key: " <> entry.key)
  let variant = entryVariant entry
  tripCategory <- variantValue tripCategoryDim variant
  fcmSubCategory <- variantValue fcmSubCategoryDim variant
  ownRows <- QMPN.findAllByMerchantOpCityIdAndMessageKey merchantOpCity.id entry.key
  let rows = filter ((== variant) . pushVariant) ownRows
  now <- getCurrentTime
  let setField pn cell = if cell.field == titleField then pn {DMPN.title = cell.text} else pn {DMPN.body = cell.text}
      setFields pn = List.foldl' setField pn cells
  case find ((== entry.language) . (.language)) rows of
    Just pn -> QMPN.updateByPrimaryKey (setFields pn)
    Nothing -> do
      legacyRows <- maybe (pure []) (QMPN.findAllByMerchantOpCityIdAndMessageKey merchantOpCity.id) (legacyKeyName key)
      let allRows = ownRows <> legacyRows
      -- A sender whose rows are all per variant reads the variant's row only, so an untagged row
      -- would never be sent. A variant no row has is not created here either: the sender would read
      -- it before the base rows of every other language, which Config Pilot owns.
      when (null variant && not (null allRows) && all (not . null . pushVariant) allRows) $
        throwError $ InvalidRequest $ entry.key <> " is stored per variant in this city; save the text of a variant"
      unless (null variant || any ((== variant) . pushVariant) allRows) $
        throwError $ InvalidRequest $ entry.key <> " has no row of this variant in this city; add the variant's row in the push notification config first"
      -- A new row under the key's name starts from the row the sender reads: the same language
      -- under the legacy name, else the English row of the same variant, else the base English row;
      -- the sender needs the notification type from it. A key with no row yet starts from the type
      -- its sender passes in code. Without a same-language row both texts have to be sent, or the
      -- new row would carry the English text in the cell that was not edited.
      let sameLanguage = find (\pn -> pn.language == entry.language && pushVariant pn == variant) legacyRows
          english ofVariant = find (\pn -> pn.language == ENGLISH && pushVariant pn == ofVariant) allRows
      when (isNothing sameLanguage) $
        forM_ [titleField, bodyField] $ \field ->
          unless (any ((== field) . (.field)) cells) $
            throwError $ InvalidRequest $ "The first " <> show entry.language <> " row of " <> entry.key <> " needs its " <> field <> " too"
      (fcmNotificationType, notificationCategory, shouldTrigger, title, body) <- case sameLanguage <|> english variant <|> english [] of
        Just pn -> pure (pn.fcmNotificationType, pn.notificationCategory, pn.shouldTrigger, pn.title, pn.body)
        Nothing -> do
          fcmNotificationType <- pushNotificationType key & fromMaybeM (InvalidRequest $ "The notification type of " <> entry.key <> " is set per row; add its first row with the type")
          notificationCategory <- Notify.deriveNotificationCategory merchantOpCity.id fcmNotificationType
          pure (fcmNotificationType, notificationCategory, True, "", "")
      id <- generateGUID
      QMPN.create $
        setFields
          DMPN.MerchantPushNotification
            { body,
              fcmNotificationType,
              fcmSubCategory,
              id,
              key = entry.key,
              language = entry.language,
              merchantId = merchantOpCity.merchantId,
              merchantOperatingCityId = merchantOpCity.id,
              notificationCategory,
              shouldTrigger,
              title,
              tripCategory,
              createdAt = now,
              updatedAt = now
            }
  CQMPN.clearCache merchantOpCity.id entry.key tripCategory

-- ---------------------------------------------------------------------------
-- merchant_message: a row per (key, language)
-- ---------------------------------------------------------------------------

smsDomain :: Domain
smsDomain =
  Domain
    { domainName = "Sms",
      domainFields = [messageField],
      domainDimensions = [],
      domainKeys = const (pure (map keyRef (allKeys @SmsKey))),
      readRows = const readSms,
      writeEntry = mapM_ . writeSms
    }

readSms :: Id DMOC.MerchantOperatingCity -> Flow [Row]
readSms cityId = do
  rows <- CQMM.findAllByMerchantOpCityId cityId Nothing
  pure [Row (keyName (Sms_Body mm.messageKey)) [] mm.language messageField mm.message Common.City | mm <- rows]

writeSms :: DMOC.MerchantOperatingCity -> Common.TranslationUpsertEntry -> Flow ()
writeSms merchantOpCity entry = do
  Sms_Body messageKey <- parseKey entry.key & fromMaybeM (InvalidRequest $ "Unknown SMS key: " <> entry.key)
  -- The page edits the text of a row that exists. Each language row carries its own provider
  -- template id (registered per text in India), which the page does not have, so a new language
  -- row comes from the merchant message upsert with that id, never as a copy of the English row.
  mm <-
    QMM.findByMerchantOperatingCityIdAndMessageKeyLanguage merchantOpCity.id messageKey entry.language
      >>= fromMaybeM (InvalidRequest $ "No " <> show entry.language <> " row of " <> entry.key <> " in this city; create it with the merchant message upsert and its template id")
  QMM.updateByPrimaryKey mm {DMM.message = entry.text}
  CQMM.clearCache merchantOpCity.id messageKey
  CQMM.clearCacheById merchantOpCity.id
