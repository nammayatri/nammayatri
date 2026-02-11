{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.EntityInfo
  ( getEntityInfoList,
    postEntityInfoUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.EntityInfo as Common
import qualified Data.List as DL
import qualified Domain.Types.EntityInfo as DEI
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.EntityInfo (create, deleteAllByEntityIdAndType)
import qualified Storage.Queries.EntityInfoExtra as QEI
import Tools.Error (GenericError (InvalidRequest))

getEntityInfoList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Text ->
  Environment.Flow Common.EntityExtraInformation
getEntityInfoList merchantShortId opCity entityType entityId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  entityInfo <- map convertEntityInfoToEntityInfoAPIEntity <$> QEI.findAllByEntityIdTypeAndOpCity entityId entityType merchant.id merchantOpCityId
  pure $ Common.EntityExtraInformation {entityType = entityType, entityId = entityId, entityInfo = entityInfo}
  where
    convertEntityInfoToEntityInfoAPIEntity DEI.EntityInfo {questionId, question, answer} =
      Common.EntityInfoAPIEntity
        { questionId = questionId,
          question = question,
          answer = answer
        }

postEntityInfoUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateEntityInfoReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postEntityInfoUpdate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let questionIds = req.newInfo <&> (.questionId)
  unless (length (DL.nub questionIds) == length questionIds) $
    throwError (InvalidRequest "questionId should be unique")
  unless (null req.newInfo) $ do
    deleteAllByEntityIdAndType req.entityId req.entityType merchant.id
    forM_ req.newInfo $ updatedEntityInfo req.entityId req.entityType merchant.id (Just merchantOpCityId) now
  pure Kernel.Types.APISuccess.Success
  where
    updatedEntityInfo entityId entityType merchantId mbMerchantOpCityId now Common.EntityInfoAPIEntity {..} =
      create $
        DEI.EntityInfo
          { entityId = entityId,
            entityType = entityType,
            questionId = questionId,
            question = question,
            answer = answer,
            merchantId = merchantId,
            merchantOperatingCityId = mbMerchantOpCityId,
            createdAt = now,
            updatedAt = now
          }
