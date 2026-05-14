{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module MerchantDocuments.Domain.Action.UI.MerchantDocument
  ( getMerchantDocument,
    listMerchantDocuments,
    createMerchantDocument,
    updateMerchantDocument,
    deleteMerchantDocument,
  )
where

import qualified Data.Map.Strict as Map
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified MerchantDocuments.Domain.Types.Common as Common
import qualified MerchantDocuments.Domain.Types.MerchantDocument as DMD
import qualified MerchantDocuments.Storage.BeamFlow as BeamFlow
import qualified MerchantDocuments.Storage.CachedQueries.MerchantDocument as CQMD
import qualified MerchantDocuments.Storage.Queries.MerchantDocument as Q
import MerchantDocuments.Tools.Error

getMerchantDocument ::
  (BeamFlow.BeamFlow m r) =>
  Id Common.Merchant ->
  Maybe (Id Common.MerchantOperatingCity) ->
  Text ->
  DMD.Role ->
  Language ->
  m DMD.MerchantDocument
getMerchantDocument mid mocId docType role lang =
  CQMD.findResolved mid mocId docType role lang
    >>= fromMaybeM (MerchantDocumentNotFound $ docType <> "/" <> show role <> "/" <> show lang)

listMerchantDocuments ::
  (BeamFlow.BeamFlow m r) =>
  Id Common.Merchant ->
  Maybe (Id Common.MerchantOperatingCity) ->
  DMD.Role ->
  Maybe Text ->
  Maybe Language ->
  m [DMD.MerchantDocument]
listMerchantDocuments mid mbMocId role mbType mbLang = do
  defaultDocs <- CQMD.findAllByMerchantIdAndRoleCached mid Nothing role
  cityDocs <- case mbMocId of
    Nothing -> pure []
    Just _ -> CQMD.findAllByMerchantIdAndRoleCached mid mbMocId role
  let byKey d = ((d.documentType, d.language), d)
      deduped = Map.elems $ Map.fromList (map byKey defaultDocs <> map byKey cityDocs)
  pure $
    filter
      ( \d ->
          maybe True (== d.documentType) mbType
            && maybe True (== d.language) mbLang
      )
      deduped

createMerchantDocument ::
  (BeamFlow.BeamFlow m r) =>
  Id Common.Merchant ->
  Maybe (Id Common.MerchantOperatingCity) ->
  Text ->
  DMD.Role ->
  Language ->
  Text ->
  Text ->
  Maybe DMD.PlatformType ->
  m DMD.MerchantDocument
createMerchantDocument mid mocId docType role lang url title mbPlatformType = do
  existing <- Q.findByMerchantIdRoleTypeCityLang mid mocId docType role lang
  whenJust existing $ \_ ->
    throwError $
      MerchantDocumentAlreadyExists $
        docType <> "/" <> show role <> "/" <> show lang <> "/" <> maybe "merchant-default" getId mocId
  newId <- generateGUID
  now <- getCurrentTime
  let doc =
        DMD.MerchantDocument
          { id = newId,
            merchantId = mid,
            merchantOperatingCityId = mocId,
            documentType = docType,
            role = role,
            language = lang,
            url = url,
            title = title,
            platformType = mbPlatformType,
            createdAt = now,
            updatedAt = now
          }
  CQMD.create doc
  pure doc

updateMerchantDocument ::
  (BeamFlow.BeamFlow m r) =>
  Id Common.Merchant ->
  Id DMD.MerchantDocument ->
  Text ->
  Text ->
  m DMD.MerchantDocument
updateMerchantDocument merchantId docId url title = do
  doc <- CQMD.findById docId >>= fromMaybeM (MerchantDocumentNotFound docId.getId)
  unless (doc.merchantId == merchantId) $ throwError (MerchantDocumentNotFound docId.getId)
  CQMD.updateUrlAndTitle url title doc
  now <- getCurrentTime
  pure doc {DMD.url = url, DMD.title = title, DMD.updatedAt = now}

deleteMerchantDocument ::
  (BeamFlow.BeamFlow m r) =>
  Id Common.Merchant ->
  Id DMD.MerchantDocument ->
  m ()
deleteMerchantDocument merchantId docId = do
  doc <- CQMD.findById docId >>= fromMaybeM (MerchantDocumentNotFound docId.getId)
  unless (doc.merchantId == merchantId) $ throwError (MerchantDocumentNotFound docId.getId)
  CQMD.deleteById doc
