{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Notification where

import Data.Time (UTCTime (UTCTime, utctDay), secondsToDiffTime)
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Notification as Domain
import Kernel.Beam.Functions (createWithKV, findAllWithKV, findAllWithOptionsKV, findOneWithKV, updateWithKV)
import Kernel.External.Payment.Interface.Types (MandateNotificationRes, NotificationStatus (NOTIFICATION_CREATED, NOTIFICATION_FAILURE, PENDING))
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Notification as BeamI hiding (Id)
import qualified Storage.Queries.DriverFee as QDF

create :: KvDbFlow m r => Domain.Notification -> m ()
create = createWithKV

findById :: KvDbFlow m r => Id Domain.Notification -> m (Maybe Domain.Notification)
findById (Id notificationId) = findOneWithKV [Se.Is BeamI.id $ Se.Eq notificationId]

findByShortId :: KvDbFlow m r => Text -> m (Maybe Domain.Notification)
findByShortId shortId = findOneWithKV [Se.Is BeamI.shortId $ Se.Eq shortId]

findAllByDriverFeeIdAndStatus :: KvDbFlow m r => [Id DF.DriverFee] -> NotificationStatus -> m [Domain.Notification]
findAllByDriverFeeIdAndStatus driverFeeIds status = findAllWithKV [Se.And [Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds), Se.Is BeamI.status $ Se.Eq status]]

findAllByStatusWithLimit ::
  KvDbFlow m r =>
  [NotificationStatus] ->
  Id DMOC.MerchantOperatingCity ->
  Int ->
  m [Domain.Notification]
findAllByStatusWithLimit status merchantOperatingCityId limit = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * 3 * 3600 * 24) endTime
  let lastCheckedAt = UTCTime (utctDay endTime) (secondsToDiffTime 0)
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamI.status $ Se.In status,
          Se.Is BeamI.createdAt $ Se.LessThanOrEq endTime,
          Se.Is BeamI.createdAt $ Se.GreaterThanOrEq startTime,
          Se.Is BeamI.merchantOperatingCityId $ Se.Eq $ Just $ getId merchantOperatingCityId,
          Se.Or
            [ Se.Is BeamI.lastStatusCheckedAt $ Se.Eq Nothing,
              Se.Is BeamI.lastStatusCheckedAt $ Se.Not (Se.Eq $ Just lastCheckedAt)
            ]
        ]
    ]
    (Se.Desc BeamI.createdAt)
    (Just limit)
    Nothing

updatePendingToFailed ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ()
updatePendingToFailed merchantOperatingCityId = do
  endTime <- getCurrentTime
  let startTime = addUTCTime (-1 * 3 * 3600 * 24) endTime
  updateWithKV
    [Se.Set BeamI.status NOTIFICATION_FAILURE]
    [ Se.And
        [ Se.Is BeamI.status $ Se.In [NOTIFICATION_CREATED, PENDING],
          Se.Is BeamI.merchantOperatingCityId $ Se.Eq (Just $ getId merchantOperatingCityId),
          Se.Is BeamI.createdAt $ Se.LessThan startTime
        ]
    ]

updateNotificationStatusAndResponseInfoById :: KvDbFlow m r => Id Domain.Notification -> Payment.NotificationStatus -> Maybe Text -> Maybe Text -> m ()
updateNotificationStatusAndResponseInfoById notificationId notificationStatus responseCode responseMessage = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.status notificationStatus,
      Se.Set BeamI.responseCode responseCode,
      Se.Set BeamI.responseMessage responseMessage,
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId notificationId)]

updateLastCheckedOn :: KvDbFlow m r => [Id Domain.Notification] -> m ()
updateLastCheckedOn notificationIds = do
  now <- getCurrentTime
  let lastCheckedAt = UTCTime (utctDay now) (secondsToDiffTime 0)
  updateWithKV
    [ Se.Set BeamI.lastStatusCheckedAt (Just lastCheckedAt),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.In $ getId <$> notificationIds)]

updateNotificationResponseById :: KvDbFlow m r => Id Domain.Notification -> MandateNotificationRes -> m ()
updateNotificationResponseById notificationId response = do
  now <- getCurrentTime
  mNotification <- findById notificationId
  let notificationStatus = maybe response.status (\nf -> if nf.status == Payment.SUCCESS then Payment.SUCCESS else response.status) mNotification
  updateWithKV
    [ Se.Set BeamI.juspayProvidedId response.juspayProvidedId,
      Se.Set BeamI.txnDate (fromMaybe now response.sourceInfo.txnDate),
      Se.Set BeamI.providerName response.providerName,
      Se.Set BeamI.notificationType response.notificationType,
      Se.Set BeamI.description response.description,
      Se.Set BeamI.status notificationStatus,
      Se.Set BeamI.dateCreated (fromMaybe now response.dateCreated),
      Se.Set BeamI.lastUpdated (fromMaybe now response.lastUpdated),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId notificationId)]

updateMerchantOperatingCityIdByNotificationId :: KvDbFlow m r => Id Domain.Notification -> Id DMOC.MerchantOperatingCity -> m ()
updateMerchantOperatingCityIdByNotificationId notificationId merchantOperatingCityId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.merchantOperatingCityId (Just $ getId merchantOperatingCityId),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId notificationId)]

instance FromTType' BeamI.Notification Domain.Notification where
  fromTType' BeamI.NotificationT {..} = do
    merchantOperatingCityId' <- case merchantOperatingCityId of
      Just opcity -> return $ Id opcity
      Nothing -> do
        dfee <- QDF.findById (Id driverFeeId) >>= fromMaybeM (DriverFeeNotFound driverFeeId)
        updateMerchantOperatingCityIdByNotificationId (Id id) (DF.merchantOperatingCityId dfee)
        return $ dfee.merchantOperatingCityId
    pure $
      Just
        Notification
          { id = Id id,
            shortId,
            sourceAmount,
            txnDate,
            mandateId = Id mandateId,
            driverFeeId = Id driverFeeId,
            juspayProvidedId,
            providerName,
            notificationType,
            description,
            status,
            dateCreated,
            lastUpdated,
            responseCode,
            responseMessage,
            lastStatusCheckedAt,
            merchantOperatingCityId = merchantOperatingCityId',
            createdAt,
            updatedAt
          }

instance ToTType' BeamI.Notification Domain.Notification where
  toTType' Notification {..} = do
    BeamI.NotificationT
      { BeamI.id = id.getId,
        BeamI.shortId = shortId,
        BeamI.sourceAmount = sourceAmount,
        BeamI.txnDate = txnDate,
        BeamI.juspayProvidedId = juspayProvidedId,
        BeamI.notificationType = notificationType,
        BeamI.description = description,
        BeamI.status = status,
        BeamI.driverFeeId = driverFeeId.getId,
        BeamI.mandateId = mandateId.getId,
        BeamI.providerName = providerName,
        BeamI.dateCreated = dateCreated,
        BeamI.lastUpdated = lastUpdated,
        BeamI.createdAt = createdAt,
        BeamI.responseCode = responseCode,
        BeamI.responseMessage = responseMessage,
        BeamI.merchantOperatingCityId = Just merchantOperatingCityId.getId,
        BeamI.lastStatusCheckedAt = lastStatusCheckedAt,
        BeamI.updatedAt = updatedAt
      }
