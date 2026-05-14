{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module MerchantDocuments.Storage.CachedQueries.MerchantDocument
  ( findResolved,
    findAllByMerchantIdAndRoleCached,
    findById,
    create,
    updateUrlAndTitle,
    deleteById,
  )
where

import Control.Applicative ((<|>))
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified MerchantDocuments.Domain.Types.Common as Common
import qualified MerchantDocuments.Domain.Types.MerchantDocument as DMD
import qualified MerchantDocuments.Storage.BeamFlow as BeamFlow
import qualified MerchantDocuments.Storage.Queries.MerchantDocument as Q

findById :: (BeamFlow.BeamFlow m r) => Id DMD.MerchantDocument -> m (Maybe DMD.MerchantDocument)
findById docId =
  Hedis.safeGet (makeIdKey docId) >>= \case
    Just doc -> pure (Just doc)
    Nothing -> do
      mbDoc <- Q.findById docId
      whenJust mbDoc $ \doc -> cacheDoc doc
      pure mbDoc

findAllByMerchantIdAndRoleCached :: (BeamFlow.BeamFlow m r) => Id Common.Merchant -> Maybe (Id Common.MerchantOperatingCity) -> DMD.Role -> m [DMD.MerchantDocument]
findAllByMerchantIdAndRoleCached mid mocId role =
  Hedis.safeGet (makeMerchantRoleKey mid mocId role) >>= \case
    Just docs -> pure docs
    Nothing -> do
      docs <- Q.findAllByMerchantIdCityAndRole mid mocId role
      cacheByMerchantRole mid mocId role docs
      pure docs

findByMerchantIdRoleTypeCached :: (BeamFlow.BeamFlow m r) => Id Common.Merchant -> Maybe (Id Common.MerchantOperatingCity) -> Text -> DMD.Role -> m [DMD.MerchantDocument]
findByMerchantIdRoleTypeCached mid mocId docType role =
  Hedis.safeGet (makeMerchantDocTypeRoleKey mid mocId docType role) >>= \case
    Just docs -> pure docs
    Nothing -> do
      docs <- Q.findByMerchantIdCityRoleType mid mocId docType role
      cacheByMerchantDocTypeRole mid mocId docType role docs
      pure docs

findResolved ::
  (BeamFlow.BeamFlow m r) =>
  Id Common.Merchant ->
  Maybe (Id Common.MerchantOperatingCity) ->
  Text ->
  DMD.Role ->
  Language ->
  m (Maybe DMD.MerchantDocument)
findResolved mid mocId docType role lang = do
  defaultDocs <- findByMerchantIdRoleTypeCached mid Nothing docType role
  cityDocs <- case mocId of
    Nothing -> pure []
    Just _ -> findByMerchantIdRoleTypeCached mid mocId docType role
  let matchLang l ds = find (\d -> d.language == l) ds
  pure $
    matchLang lang cityDocs
      <|> matchLang lang defaultDocs
      <|> matchLang ENGLISH cityDocs
      <|> matchLang ENGLISH defaultDocs

create :: (BeamFlow.BeamFlow m r) => DMD.MerchantDocument -> m ()
create doc = do
  Q.create doc
  clearCache doc

updateUrlAndTitle :: (BeamFlow.BeamFlow m r) => Text -> Text -> DMD.MerchantDocument -> m ()
updateUrlAndTitle url title doc = do
  Q.updateUrlAndTitle url title doc.id
  clearCache doc

deleteById :: (BeamFlow.BeamFlow m r) => DMD.MerchantDocument -> m ()
deleteById doc = do
  Q.deleteById doc.id
  clearCache doc

clearCache :: (BeamFlow.BeamFlow m r) => DMD.MerchantDocument -> m ()
clearCache doc = do
  let keys = [makeIdKey doc.id, makeMerchantRoleKey doc.merchantId doc.merchantOperatingCityId doc.role, makeMerchantDocTypeRoleKey doc.merchantId doc.merchantOperatingCityId doc.documentType doc.role]
  let defaultKeys = case doc.merchantOperatingCityId of
        Just _ -> [makeMerchantRoleKey doc.merchantId Nothing doc.role, makeMerchantDocTypeRoleKey doc.merchantId Nothing doc.documentType doc.role]
        Nothing -> []
  let allKeys = keys <> defaultKeys
  Hedis.withCrossAppRedis $ forM_ allKeys Hedis.del
  Hedis.runInMultiCloudRedisWrite $ forM_ allKeys Hedis.del

cacheDoc :: (CacheFlow m r) => DMD.MerchantDocument -> m ()
cacheDoc doc = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeIdKey doc.id) doc expTime

cacheByMerchantRole :: (CacheFlow m r) => Id Common.Merchant -> Maybe (Id Common.MerchantOperatingCity) -> DMD.Role -> [DMD.MerchantDocument] -> m ()
cacheByMerchantRole mid mocId role docs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantRoleKey mid mocId role) docs expTime

cacheByMerchantDocTypeRole :: (CacheFlow m r) => Id Common.Merchant -> Maybe (Id Common.MerchantOperatingCity) -> Text -> DMD.Role -> [DMD.MerchantDocument] -> m ()
cacheByMerchantDocTypeRole mid mocId docType role docs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMerchantDocTypeRoleKey mid mocId docType role) docs expTime

makeIdKey :: Id DMD.MerchantDocument -> Text
makeIdKey docId = "CachedQueries:MerchantDocument:Id-" <> docId.getId

mocIdText :: Maybe (Id Common.MerchantOperatingCity) -> Text
mocIdText = maybe "" (.getId)

makeMerchantRoleKey :: Id Common.Merchant -> Maybe (Id Common.MerchantOperatingCity) -> DMD.Role -> Text
makeMerchantRoleKey mid mocId role = "CachedQueries:MerchantDocument:MerchantId-" <> mid.getId <> ":MocId-" <> mocIdText mocId <> ":Role-" <> show role

makeMerchantDocTypeRoleKey :: Id Common.Merchant -> Maybe (Id Common.MerchantOperatingCity) -> Text -> DMD.Role -> Text
makeMerchantDocTypeRoleKey mid mocId docType role = "CachedQueries:MerchantDocument:MerchantId-" <> mid.getId <> ":MocId-" <> mocIdText mocId <> ":DocumentType-" <> docType <> ":Role-" <> show role
