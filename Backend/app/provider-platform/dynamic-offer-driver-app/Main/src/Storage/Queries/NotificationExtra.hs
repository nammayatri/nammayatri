module Storage.Queries.NotificationExtra where

import Data.Time (UTCTime (UTCTime, utctDay), secondsToDiffTime)
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Notification as Domain
import Kernel.Beam.Functions
import Kernel.External.Payment.Interface.Types as PaymentI
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Notification as BeamI
import Storage.Queries.OrphanInstances.Notification ()

-- Extra code goes here --
findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Notification -> m (Maybe Domain.Notification)
findById (Id notificationId) = findOneWithKV [Se.Is BeamI.id $ Se.Eq notificationId]

updateMerchantOperatingCityIdByNotificationId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Notification -> Id DMOC.MerchantOperatingCity -> m ()
updateMerchantOperatingCityIdByNotificationId notificationId merchantOperatingCityId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamI.merchantOperatingCityId (Just $ getId merchantOperatingCityId),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.Eq $ getId notificationId)]

findAllByDriverFeeIdAndStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DF.DriverFee] -> NotificationStatus -> m [Domain.Notification]
findAllByDriverFeeIdAndStatus driverFeeIds status = findAllWithKV [Se.And [Se.Is BeamI.driverFeeId $ Se.In (getId <$> driverFeeIds), Se.Is BeamI.status $ Se.Eq status]]

findAllByStatusWithLimit ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
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
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
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

updateLastCheckedOn :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Domain.Notification] -> m ()
updateLastCheckedOn notificationIds = do
  now <- getCurrentTime
  let lastCheckedAt = UTCTime (utctDay now) (secondsToDiffTime 0)
  updateWithKV
    [ Se.Set BeamI.lastStatusCheckedAt (Just lastCheckedAt),
      Se.Set BeamI.updatedAt now
    ]
    [Se.Is BeamI.id (Se.In $ getId <$> notificationIds)]

updateNotificationResponseById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Notification -> PaymentI.MandateNotificationRes -> m ()
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
