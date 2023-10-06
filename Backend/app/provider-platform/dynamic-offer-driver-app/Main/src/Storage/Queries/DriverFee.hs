{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverFee where

import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as Domain
import Domain.Types.Merchant
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, MonadFlow, Money)
import Kernel.Types.Id
import Kernel.Types.Time
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF

create :: MonadFlow m => DriverFee -> m ()
create = createWithKV

createMany :: MonadFlow m => [DriverFee] -> m ()
createMany = traverse_ create

findById :: MonadFlow m => Id DriverFee -> m (Maybe DriverFee)
findById (Id driverFeeId) = findOneWithKV [Se.Is BeamDF.id $ Se.Eq driverFeeId]

findPendingFeesByDriverFeeId :: MonadFlow m => Id DriverFee -> m (Maybe DriverFee)
findPendingFeesByDriverFeeId (Id driverFeeId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.id $ Se.Eq driverFeeId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]
        ]
    ]

findPendingFeesByDriverId :: MonadFlow m => Id Driver -> m [DriverFee]
findPendingFeesByDriverId (Id driverId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

findLatestFeeByDriverId :: MonadFlow m => Id Driver -> m (Maybe DriverFee)
findLatestFeeByDriverId (Id driverId) =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.feeType $ Se.Not (Se.Eq MANDATE_REGISTRATION),
          Se.Is BeamDF.status (Se.Eq ONGOING)
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findLatestRegisterationFeeByDriverId :: MonadFlow m => Id Driver -> m (Maybe DriverFee)
findLatestRegisterationFeeByDriverId (Id driverId) =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.feeType (Se.Eq MANDATE_REGISTRATION),
          Se.Is BeamDF.status (Se.Eq PAYMENT_PENDING)
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findOldestFeeByStatus :: MonadFlow m => Id Driver -> DriverFeeStatus -> m (Maybe DriverFee)
findOldestFeeByStatus (Id driverId) status =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.Eq status
        ]
    ]
    (Se.Asc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findFeesInRangeWithStatus :: MonadFlow m => Maybe (Id Merchant) -> UTCTime -> UTCTime -> DriverFeeStatus -> Maybe Int -> m [DriverFee] -- remove maybe from merchantId later
findFeesInRangeWithStatus mbMerchantId startTime endTime status mbLimit =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Or [Se.Is BeamDF.status (Se.Eq ONGOING), Se.Is BeamDF.payBy (Se.LessThanOrEq endTime)],
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
          <> [Se.Is BeamDF.merchantId $ Se.Eq $ getId (fromJust mbMerchantId) | isJust mbMerchantId]
    ]
    (Se.Desc BeamDF.endTime)
    mbLimit
    Nothing

findAllFeesInRangeWithStatus :: MonadFlow m => Maybe (Id Merchant) -> UTCTime -> UTCTime -> DriverFeeStatus -> Maybe Int -> m [DriverFee] -- remove maybe from merchantId later
findAllFeesInRangeWithStatus mbMerchantId startTime endTime status mbLimit =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Is BeamDF.feeType $ Se.In [RECURRING_EXECUTION_INVOICE, RECURRING_INVOICE]
        ]
          <> [Se.Is BeamDF.merchantId $ Se.Eq $ getId (fromJust mbMerchantId) | isJust mbMerchantId]
    ]
    (Se.Desc BeamDF.endTime)
    mbLimit
    Nothing

findFeesInRangeAndDriverIdsWithStatus :: MonadFlow m => UTCTime -> UTCTime -> DriverFeeStatus -> [Id Person] -> m [DriverFee]
findFeesInRangeAndDriverIdsWithStatus startTime endTime status driverIds =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE,
          Se.Is BeamDF.driverId $ Se.In (getId <$> driverIds)
        ]
    ]

findFeeInRangeAndDriverId :: MonadFlow m => UTCTime -> UTCTime -> Id Person -> m [DriverFee]
findFeeInRangeAndDriverId startTime endTime driverId = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.platformFee $ Se.GreaterThanOrEq 1.0,
          Se.Is BeamDF.driverId $ Se.Eq (getId driverId),
          Se.Is BeamDF.feeType $ Se.Not (Se.Eq MANDATE_REGISTRATION)
        ]
    ]

findWindowsWithStatus :: MonadFlow m => Id Person -> UTCTime -> UTCTime -> Maybe DriverFeeStatus -> Int -> Int -> m [DriverFee]
findWindowsWithStatus (Id driverId) from to mbStatus limitVal offsetVal =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.endTime $ Se.GreaterThanOrEq from,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq to,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
          <> [Se.Is BeamDF.status $ Se.Eq $ fromJust mbStatus | isJust mbStatus]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just limitVal)
    (Just offsetVal)

findWindows :: MonadFlow m => Id Person -> UTCTime -> UTCTime -> Int -> Int -> m [DriverFee]
findWindows (Id driverId) from to limitVal offsetVal =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.createdAt $ Se.GreaterThanOrEq from,
          Se.Is BeamDF.createdAt $ Se.LessThanOrEq to
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just limitVal)
    (Just offsetVal)

findOngoingAfterEndTime :: MonadFlow m => Id Person -> UTCTime -> m (Maybe DriverFee)
findOngoingAfterEndTime (Id driverId) now =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.Eq ONGOING,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq now,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

-- findDriverFeeInRangeWithNotifcationNotSentAndStatus :: MonadFlow m => Integer -> UTCTime -> UTCTime -> Domain.DriverFeeStatus -> m [DriverFee]
-- findDriverFeeInRangeWithNotifcationNotSentAndStatus limit startTime endTime status = do
--   dbConf <- getMasterBeamConfig
--   res <- L.runDB dbConf $
--     L.findRows $
--       B.select $
--         B.limit_ limit $
--           B.filter_
--             ( \(driverFee, notification) ->
--                 notification.driverFeeId B./=. driverFee.id
--                   B.&&. driverFee.createdAt B.>=. B.val_ startTime
--                   B.&&. driverFee.createdAt B.<=. B.val_ endTime
--                   B.&&. driverFee.status B.==. B.val_ status
--             )
--             do
--               driverFee <- B.all_ (SBC.driverFee SBC.atlasDB)
--               notifications <- B.join_' (SBC.notification SBC.atlasDB) (\notification'' -> BeamN.driverFeeId notification'' B.==?. BeamDF.id driverFee)
--               pure (driverFee, notifications)
--   case res of
--     Right res' -> do
--       let driverFee' = fst <$> res'
--       catMaybes <$> mapM fromTType' driverFee'
--     Left _ -> pure []

findDriverFeeInRangeWithNotifcationNotSentAndStatus :: MonadFlow m => Id Merchant -> Int -> UTCTime -> UTCTime -> Domain.DriverFeeStatus -> m [DriverFee]
findDriverFeeInRangeWithNotifcationNotSentAndStatus merchantId limit startTime endTime status = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE,
          Se.Is BeamDF.autopayPaymentStage $ Se.Eq (Just NOTIFICATION_SCHEDULED),
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just limit)
    Nothing

findDriverFeeInRangeWithOrderNotExecutedAndPending :: MonadFlow m => Id Merchant -> Int -> UTCTime -> UTCTime -> m [DriverFee]
findDriverFeeInRangeWithOrderNotExecutedAndPending merchantId limit startTime endTime = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE,
          Se.Is BeamDF.autopayPaymentStage $ Se.Eq (Just EXECUTION_SCHEDULED),
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just limit)
    Nothing

findMaxBillNumberInRange :: MonadFlow m => Id Merchant -> UTCTime -> UTCTime -> m [DriverFee]
findMaxBillNumberInRange merchantId startTime endTime =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId
        ]
    ]
    (Se.Desc BeamDF.billNumber)
    (Just 1)
    Nothing

findUnpaidAfterPayBy :: MonadFlow m => Id Person -> UTCTime -> m (Maybe DriverFee)
findUnpaidAfterPayBy (Id driverId) now =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.payBy $ Se.LessThanOrEq now,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

updateFee :: MonadFlow m => Id DriverFee -> Maybe Money -> Money -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> UTCTime -> Bool -> m ()
updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now isRideEnd = do
  driverFeeObject <- findById driverFeeId
  case driverFeeObject of
    Just df -> do
      let govtCharges' = df.govtCharges
      let platformFee' = df.platformFee.fee
      let cgst' = df.platformFee.cgst
      let sgst' = df.platformFee.sgst
      let totalEarnings = df.totalEarnings
      let numRides = df.numRides + if isRideEnd then 1 else 0
      let fare = fromMaybe 0 mbFare
      updateOneWithKV
        [ Se.Set BeamDF.govtCharges $ govtCharges' + govtCharges,
          Se.Set BeamDF.platformFee $ platformFee' + platformFee,
          Se.Set BeamDF.cgst $ cgst' + cgst,
          Se.Set BeamDF.sgst $ sgst' + sgst,
          Se.Set BeamDF.totalEarnings $ totalEarnings + fare,
          Se.Set BeamDF.numRides numRides,
          Se.Set BeamDF.updatedAt now
        ]
        [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]
    Nothing -> pure ()

resetFee :: MonadFlow m => Id DriverFee -> Money -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney -> UTCTime -> m ()
resetFee driverFeeId govtCharges platformFee cgst sgst now = do
  updateOneWithKV
    [ Se.Set BeamDF.govtCharges govtCharges,
      Se.Set BeamDF.platformFee platformFee,
      Se.Set BeamDF.cgst cgst,
      Se.Set BeamDF.sgst sgst,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]

updateOfferId :: MonadFlow m => Maybe Text -> Id DriverFee -> UTCTime -> m ()
updateOfferId offerId driverFeeId now = do
  updateOneWithKV
    [Se.Set BeamDF.offerId offerId, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

updateOfferAndPlanDetails :: MonadFlow m => Maybe Text -> Maybe Text -> Id DriverFee -> UTCTime -> m ()
updateOfferAndPlanDetails offerId planAndOfferTitle driverFeeId now = do
  updateOneWithKV
    [Se.Set BeamDF.offerId offerId, Se.Set BeamDF.planOfferTitle planAndOfferTitle, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

updateAutopayPaymentStageById :: MonadFlow m => Maybe Domain.AutopayPaymentStage -> Id DriverFee -> m ()
updateAutopayPaymentStageById autopayPaymentStage driverFeeId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDF.autopayPaymentStage autopayPaymentStage, Se.Set BeamDF.stageUpdatedAt (Just now)]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

updateAutopayPaymentStageByIds :: MonadFlow m => Maybe Domain.AutopayPaymentStage -> [Id DriverFee] -> m ()
updateAutopayPaymentStageByIds autopayPaymentStage driverFeeIds = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDF.autopayPaymentStage autopayPaymentStage, Se.Set BeamDF.stageUpdatedAt (Just now)]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

updateStatusByIds :: MonadFlow m => DriverFeeStatus -> [Id DriverFee] -> UTCTime -> m ()
updateStatusByIds status driverFeeIds now =
  case status of
    CLEARED -> do
      updateWithKV
        [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now, Se.Set BeamDF.collectedAt (Just now)]
        [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]
    _ -> do
      updateWithKV
        [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
        [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

updateFeeTypeByIds :: MonadFlow m => FeeType -> [Id DriverFee] -> UTCTime -> m ()
updateFeeTypeByIds feeType driverFeeIds now =
  updateWithKV
    [ Se.Set BeamDF.feeType feeType,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

updateBillNumberById :: MonadFlow m => Maybe Int -> Id DriverFee -> m ()
updateBillNumberById billNumber driverFeeId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.billNumber billNumber,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id $ Se.Eq (driverFeeId.getId)]

updateToManualFeeByDriverFeeIds :: MonadFlow m => [Id DriverFee] -> m ()
updateToManualFeeByDriverFeeIds driverFeeIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.feeType Domain.RECURRING_INVOICE,
      Se.Set BeamDF.status Domain.PAYMENT_OVERDUE,
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds),
          Se.Is BeamDF.status $ Se.Eq Domain.PAYMENT_PENDING,
          Se.Is BeamDF.feeType $ Se.Eq Domain.RECURRING_EXECUTION_INVOICE
        ]
    ]

findAllByStatusAndDriverId :: MonadFlow m => Id Person -> [Domain.DriverFeeStatus] -> m [DriverFee]
findAllByStatusAndDriverId (Id driverId) driverFeeStatus = findAllWithKV [Se.And [Se.Is BeamDF.feeType $ Se.In [RECURRING_INVOICE], Se.Is BeamDF.status $ Se.In driverFeeStatus, Se.Is BeamDF.driverId $ Se.Eq driverId]]

findAllPendingAndDueDriverFeeByDriverId :: MonadFlow m => Id Person -> m [DriverFee]
findAllPendingAndDueDriverFeeByDriverId (Id driverId) = findAllWithKV [Se.And [Se.Is BeamDF.feeType $ Se.In [RECURRING_INVOICE, RECURRING_EXECUTION_INVOICE], Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE], Se.Is BeamDF.driverId $ Se.Eq driverId]]

findAllPendingRegistrationDriverFeeByDriverId :: MonadFlow m => Id Person -> m [DriverFee]
findAllPendingRegistrationDriverFeeByDriverId (Id driverId) = findAllWithKV [Se.And [Se.Is BeamDF.feeType $ Se.Eq MANDATE_REGISTRATION, Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING, Se.Is BeamDF.driverId $ Se.Eq driverId]]

-- add fee collection time later if req'd
findAllByVolunteerIds :: MonadFlow m => Id Merchant -> [Text] -> UTCTime -> UTCTime -> m [DriverFee]
findAllByVolunteerIds (Id merchantId) volunteerIds from to = do
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDF.merchantId $ Se.Eq merchantId,
          Se.Is BeamDF.collectedAt $ Se.GreaterThanOrEq (Just from),
          Se.Is BeamDF.collectedAt $ Se.LessThanOrEq (Just to)
        ]
          <> [Se.Is BeamDF.collectedBy $ Se.In (map Just volunteerIds)]
    ]
    (Se.Desc BeamDF.updatedAt)
    Nothing
    Nothing

findAllByStatus :: MonadFlow m => Id Merchant -> DriverFeeStatus -> UTCTime -> UTCTime -> m [DriverFee]
findAllByStatus (Id merchantId) status from to = do
  findAllWithOptionsKV
    [ Se.Is BeamDF.merchantId $ Se.Eq merchantId,
      Se.Is BeamDF.collectedAt $ Se.GreaterThanOrEq (Just from),
      Se.Is BeamDF.collectedAt $ Se.LessThanOrEq (Just to),
      Se.Is BeamDF.status $ Se.Eq status
    ]
    (Se.Desc BeamDF.updatedAt)
    Nothing
    Nothing

findAllByDriverFeeIds :: MonadFlow m => [Id DriverFee] -> m [DriverFee]
findAllByDriverFeeIds driverFeeIds = do
  findAllWithKV
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

findLatestByFeeTypeAndStatus :: MonadFlow m => Domain.FeeType -> [Domain.DriverFeeStatus] -> Id Person -> m (Maybe DriverFee)
findLatestByFeeTypeAndStatus feeType status driverId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.Eq feeType,
          Se.Is BeamDF.status $ Se.In status,
          Se.Is BeamDF.driverId $ Se.Eq driverId.getId
        ]
    ]
    (Se.Desc BeamDF.updatedAt)
    (Just 1)
    Nothing
    <&> listToMaybe

-- TODO : Merge relevant queries
findAllByTimeMerchantAndStatus :: MonadFlow m => Id Merchant -> UTCTime -> UTCTime -> [Domain.DriverFeeStatus] -> m [DriverFee]
findAllByTimeMerchantAndStatus (Id merchantId) startTime endTime status = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.merchantId $ Se.Eq merchantId,
          Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.startTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.In status
        ]
    ]

updateStatus :: MonadFlow m => DriverFeeStatus -> Id DriverFee -> UTCTime -> m ()
updateStatus status (Id driverFeeId) now = do
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

updateFeeType :: MonadFlow m => FeeType -> UTCTime -> Id DriverFee -> m ()
updateFeeType feeType now (Id driverFeeId) = do
  updateOneWithKV
    [Se.Set BeamDF.feeType feeType, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

updateAutoPayToManual :: MonadFlow m => Id DriverFee -> m ()
updateAutoPayToManual driverFeeId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.feeType RECURRING_INVOICE,
      Se.Set BeamDF.status PAYMENT_OVERDUE,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

updateManualToAutoPay :: MonadFlow m => Id DriverFee -> m ()
updateManualToAutoPay driverFeeId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.feeType RECURRING_EXECUTION_INVOICE,
      Se.Set BeamDF.status PAYMENT_PENDING,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

updateRetryCount :: MonadFlow m => Int -> UTCTime -> Id DriverFee -> m ()
updateRetryCount retryCount now (Id driverFeeId) = do
  updateOneWithKV
    [Se.Set BeamDF.schedulerTryCount retryCount, Se.Set BeamDF.updatedAt now]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

updateRegisterationFeeStatusByDriverId :: MonadFlow m => DriverFeeStatus -> Id Person -> m ()
updateRegisterationFeeStatusByDriverId status (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
    [Se.And [Se.Is BeamDF.driverId (Se.Eq driverId), Se.Is BeamDF.feeType (Se.Eq MANDATE_REGISTRATION), Se.Is BeamDF.status (Se.Eq PAYMENT_PENDING)]]

updateCollectedPaymentStatus :: MonadFlow m => DriverFeeStatus -> Maybe Text -> UTCTime -> Id DriverFee -> m ()
updateCollectedPaymentStatus status volunteerId now (Id driverFeeId) = do
  updateOneWithKV
    [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now, Se.Set BeamDF.collectedBy volunteerId, Se.Set BeamDF.collectedAt (Just now)]
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]

updateAllExecutionPendingToManualOverdueByDriverId :: MonadFlow m => Id Person -> m ()
updateAllExecutionPendingToManualOverdueByDriverId driverId = do
  updateWithKV
    [Se.Set BeamDF.feeType RECURRING_INVOICE, Se.Set BeamDF.status PAYMENT_OVERDUE]
    [Se.And [Se.Is BeamDF.driverId (Se.Eq driverId.getId), Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING, Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE]]

instance FromTType' BeamDF.DriverFee DriverFee where
  fromTType' BeamDF.DriverFeeT {..} = do
    pure $
      Just
        DriverFee
          { id = Id id,
            merchantId = Id merchantId,
            driverId = Id driverId,
            govtCharges = govtCharges,
            platformFee = Domain.PlatformFee platformFee cgst sgst,
            numRides = numRides,
            payBy = payBy,
            totalEarnings = totalEarnings,
            startTime = startTime,
            endTime = endTime,
            status = status,
            feeType = feeType,
            collectedBy = collectedBy,
            offerId = offerId,
            planOfferTitle,
            autopayPaymentStage,
            collectedAt = collectedAt,
            createdAt = createdAt,
            updatedAt = updatedAt,
            schedulerTryCount,
            billNumber,
            stageUpdatedAt,
            feeWithoutDiscount
          }

instance ToTType' BeamDF.DriverFee DriverFee where
  toTType' DriverFee {..} = do
    BeamDF.DriverFeeT
      { BeamDF.id = getId id,
        BeamDF.merchantId = getId merchantId,
        BeamDF.driverId = getId driverId,
        BeamDF.govtCharges = govtCharges,
        BeamDF.platformFee = platformFee.fee,
        BeamDF.cgst = platformFee.cgst,
        BeamDF.sgst = platformFee.sgst,
        BeamDF.numRides = numRides,
        BeamDF.payBy = payBy,
        BeamDF.totalEarnings = totalEarnings,
        BeamDF.startTime = startTime,
        BeamDF.endTime = endTime,
        BeamDF.status = status,
        BeamDF.feeType = feeType,
        BeamDF.collectedBy = collectedBy,
        BeamDF.offerId = offerId,
        BeamDF.planOfferTitle = planOfferTitle,
        BeamDF.billNumber = billNumber,
        BeamDF.autopayPaymentStage = autopayPaymentStage,
        BeamDF.stageUpdatedAt = stageUpdatedAt,
        BeamDF.feeWithoutDiscount = feeWithoutDiscount,
        BeamDF.schedulerTryCount = schedulerTryCount,
        BeamDF.collectedAt = collectedAt,
        BeamDF.createdAt = createdAt,
        BeamDF.updatedAt = updatedAt
      }
