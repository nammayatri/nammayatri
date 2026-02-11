{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.Exophone
  ( postExophoneCreate,
    getExophoneList,
    getExophone,
    postExophoneUpdate,
    deleteExophoneDelete,
  )
where

import qualified API.Types.ProviderPlatform.Management.Exophone as Common
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Merchant as DM
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Call.Types as CallTypes
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Exophone as QExophone
import Tools.Error

-- | Create a new Exophone
postExophoneCreate ::
  Id.ShortId DM.Merchant ->
  Context.City ->
  Common.CreateExophoneReq ->
  Environment.Flow Common.ExophoneRes
postExophoneCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let exophoneType = mapExophoneType req.exophoneExophoneType

  -- Check if exophone with same type already exists for this merchant operating city
  existingExophones <- QExophone.findByMerchantOpCityIdAndExophoneType merchantOpCityId exophoneType
  unless (null existingExophones) $
    throwError $ InvalidRequest $ "Exophone with type " <> show req.exophoneExophoneType <> " already exists for this merchant in this city."

  -- Check if phone numbers are already in use
  allExophones <- CQExophone.findAllExophones
  let alreadyUsedPhones = getAllPhones allExophones
  let reqPhones = [req.exophonePrimaryPhone, req.exophoneBackupPhone]
  let busyPhones = filter (`elem` alreadyUsedPhones) reqPhones
  unless (null busyPhones) $
    throwError $ InvalidRequest $ "These phones are already in use: " <> show busyPhones

  now <- getCurrentTime
  exophoneId <- generateGUID
  let exophone =
        DExophone.Exophone
          { id = exophoneId,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            primaryPhone = req.exophonePrimaryPhone,
            backupPhone = req.exophoneBackupPhone,
            isPrimaryDown = False,
            exophoneType = mapExophoneType req.exophoneExophoneType,
            callService = mapCallService req.exophoneCallService,
            createdAt = now,
            updatedAt = now
          }

  CQExophone.create exophone
  logTagInfo "dashboard -> postExophoneCreate : " (show exophone.id)
  pure $ mkExophoneRes exophone
  where
    getAllPhones es = (es <&> (.primaryPhone)) <> (es <&> (.backupPhone))

-- | List all Exophones for a merchant operating city
getExophoneList ::
  Id.ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Environment.Flow Common.ExophoneListRes
getExophoneList merchantShortId opCity _mbMerchantOpCityId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  exophones <- CQExophone.findAllByMerchantOpCityId merchantOpCityId
  pure $ Common.ExophoneListRes {exophones = mkExophoneItem <$> exophones}

-- | Get a single Exophone by ID
getExophone ::
  Id.ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Environment.Flow Common.ExophoneRes
getExophone merchantShortId opCity exophoneIdText = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let exophoneId = Id.Id exophoneIdText

  exophone <- QExophone.findByPrimaryKey exophoneId >>= fromMaybeM (InvalidRequest $ "Exophone not found: " <> exophoneIdText)

  -- Verify the exophone belongs to this merchant operating city
  unless (exophone.merchantOperatingCityId == merchantOpCityId) $
    throwError $ InvalidRequest "Exophone does not belong to this merchant operating city"

  pure $ mkExophoneRes exophone

-- | Update an Exophone
postExophoneUpdate ::
  Id.ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.UpdateExophoneReq ->
  Environment.Flow APISuccess.APISuccess
postExophoneUpdate merchantShortId opCity exophoneIdText req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let exophoneId = Id.Id exophoneIdText

  exophone <- QExophone.findByPrimaryKey exophoneId >>= fromMaybeM (InvalidRequest $ "Exophone not found: " <> exophoneIdText)

  -- Verify the exophone belongs to this merchant operating city
  unless (exophone.merchantOperatingCityId == merchantOpCityId) $
    throwError $ InvalidRequest "Exophone does not belong to this merchant operating city"

  -- Check phone uniqueness upfront (single call)
  let phonesToCheck = catMaybes
        [ if req.exophonePrimaryPhone /= Just exophone.primaryPhone then req.exophonePrimaryPhone else Nothing,
          if req.exophoneBackupPhone /= Just exophone.backupPhone then req.exophoneBackupPhone else Nothing
        ]
  unless (null phonesToCheck) $ do
    allExophones <- CQExophone.findAllExophones
    let alreadyUsedPhones = getAllPhones $ filter (\e -> e.id /= exophoneId) allExophones
    let busyPhones = filter (`elem` alreadyUsedPhones) phonesToCheck
    unless (null busyPhones) $
      throwError $ InvalidRequest $ "These phones are already in use: " <> show busyPhones

  now <- getCurrentTime
  let updatedExophone =
        exophone
          { DExophone.primaryPhone = fromMaybe exophone.primaryPhone req.exophonePrimaryPhone,
            DExophone.backupPhone = fromMaybe exophone.backupPhone req.exophoneBackupPhone,
            DExophone.isPrimaryDown = fromMaybe exophone.isPrimaryDown req.exophoneIsPrimaryDown,
            DExophone.updatedAt = now
          }

  QExophone.updateByPrimaryKey updatedExophone
  CQExophone.clearCache merchantOpCityId [exophone]

  logTagInfo "dashboard -> postExophoneUpdate : " (show exophoneId)
  pure APISuccess.Success
  where
    getAllPhones es = (es <&> (.primaryPhone)) <> (es <&> (.backupPhone))

-- | Delete an Exophone
deleteExophoneDelete ::
  Id.ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Environment.Flow APISuccess.APISuccess
deleteExophoneDelete merchantShortId opCity exophoneIdText = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let exophoneId = Id.Id exophoneIdText

  exophone <- QExophone.findByPrimaryKey exophoneId >>= fromMaybeM (InvalidRequest $ "Exophone not found: " <> exophoneIdText)

  -- Verify the exophone belongs to this merchant operating city
  unless (exophone.merchantOperatingCityId == merchantOpCityId) $
    throwError $ InvalidRequest "Exophone does not belong to this merchant operating city"

  -- Clear cache and delete
  CQExophone.clearCache merchantOpCityId [exophone]
  CQExophone.deleteById exophoneId

  logTagInfo "dashboard -> deleteExophoneDelete : " (show exophoneId)
  pure APISuccess.Success

-- Helper functions for type mapping
mapCallService :: Common.ExophoneCallService -> CallTypes.CallService
mapCallService Common.Exotel = CallTypes.Exotel
mapCallService Common.Knowlarity = CallTypes.Knowlarity

mapExophoneType :: Common.ExophoneExophoneType -> DExophone.ExophoneType
mapExophoneType Common.CALL_RIDE = DExophone.CALL_RIDE
mapExophoneType Common.END_RIDE = DExophone.END_RIDE
mapExophoneType Common.CALL_DELIVERY_SENDER = DExophone.CALL_DELIVERY_SENDER
mapExophoneType Common.CALL_DELIVERY_RECEIVER = DExophone.CALL_DELIVERY_RECEIVER

mapCallServiceToApi :: CallTypes.CallService -> Common.ExophoneCallService
mapCallServiceToApi CallTypes.Exotel = Common.Exotel
mapCallServiceToApi CallTypes.Knowlarity = Common.Knowlarity
mapCallServiceToApi _ = Common.Exotel -- Default to Exotel for unsupported services

mapExophoneTypeToApi :: DExophone.ExophoneType -> Common.ExophoneExophoneType
mapExophoneTypeToApi DExophone.CALL_RIDE = Common.CALL_RIDE
mapExophoneTypeToApi DExophone.END_RIDE = Common.END_RIDE
mapExophoneTypeToApi DExophone.CALL_DELIVERY_SENDER = Common.CALL_DELIVERY_SENDER
mapExophoneTypeToApi DExophone.CALL_DELIVERY_RECEIVER = Common.CALL_DELIVERY_RECEIVER

mkExophoneItem :: DExophone.Exophone -> Common.ExophoneItem
mkExophoneItem exophone =
  Common.ExophoneItem
    { exophoneId = Id.getId exophone.id,
      exophonePrimaryPhone = exophone.primaryPhone,
      exophoneBackupPhone = exophone.backupPhone,
      exophoneCallService = mapCallServiceToApi exophone.callService,
      exophoneExophoneType = mapExophoneTypeToApi exophone.exophoneType,
      exophoneIsPrimaryDown = exophone.isPrimaryDown,
      exophoneMerchantId = Id.getId exophone.merchantId,
      exophoneMerchantOperatingCityId = Id.getId exophone.merchantOperatingCityId,
      exophoneCreatedAt = exophone.createdAt,
      exophoneUpdatedAt = exophone.updatedAt
    }

mkExophoneRes :: DExophone.Exophone -> Common.ExophoneRes
mkExophoneRes exophone = Common.ExophoneRes {exophone = mkExophoneItem exophone}
