module Storage.Queries.DriverFeeExtra where

import Data.Time (Day, UTCTime (UTCTime, utctDay), addDays, fromGregorian, toGregorian)
import qualified Domain.Types.Booking as SRB
import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as Domain
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.Plan as DPlan
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common (EsqDBFlow, HighPrecMoney (..), MonadFlow)
import Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Utils.Common (fork, getLocalCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF
import Storage.Queries.OrphanInstances.DriverFee ()

-- Extra code goes here --

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DriverFee -> m (Maybe DriverFee)
findById (Id driverFeeId) = findOneWithKV [Se.Is BeamDF.id $ Se.Eq driverFeeId]

findPendingFeesByDriverFeeId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DriverFee -> m (Maybe DriverFee)
findPendingFeesByDriverFeeId (Id driverFeeId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.id $ Se.Eq driverFeeId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]
        ]
    ]

findPendingFeesByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Maybe [Id DriverFee] -> ServiceNames -> m [DriverFee]
findPendingFeesByDriverIdAndServiceName (Id driverId) mbDriverFeeIds serviceName =
  findAllWithKV
    [ Se.And $
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
          <> maybe [] (\driverFeeIds -> [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]) mbDriverFeeIds
    ]

findFeeByDriverIdAndServiceNameInRange ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Driver ->
  ServiceNames ->
  UTCTime ->
  UTCTime ->
  m (Maybe DriverFee)
findFeeByDriverIdAndServiceNameInRange (Id driverId) serviceName from to =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.startTime $ Se.Eq from,
          Se.Is BeamDF.endTime $ Se.Eq to,
          Se.Is BeamDF.feeType $ Se.Not $ Se.In [MANDATE_REGISTRATION, PAYOUT_REGISTRATION, ONE_TIME_SECURITY_DEPOSIT]
        ]
    ]

findLatestFeeByDriverIdAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Driver ->
  ServiceNames ->
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Bool ->
  m (Maybe DriverFee)
findLatestFeeByDriverIdAndServiceName (Id driverId) serviceName merchantOperatingCityId startTime endTime enableCityBasedFeeSwitch = do
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is BeamDF.driverId (Se.Eq driverId),
            Se.Is BeamDF.feeType $ Se.Not $ Se.In [MANDATE_REGISTRATION, PAYOUT_REGISTRATION, ONE_TIME_SECURITY_DEPOSIT],
            Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
            Se.Is BeamDF.status (Se.Eq ONGOING),
            Se.Is BeamDF.siblingFeeId $ Se.Eq Nothing,
            Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
            Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime
          ]
            <> [Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId) | enableCityBasedFeeSwitch]
        )
    ]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findLatestRegisterationFeeByDriverIdAndServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> ServiceNames -> m (Maybe DriverFee)
findLatestRegisterationFeeByDriverIdAndServiceName (Id driverId) serviceName =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.feeType (Se.Eq MANDATE_REGISTRATION),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.status (Se.Eq PAYMENT_PENDING)
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findAllFeesInRangeWithStatusAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe (Id Merchant) ->
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  DriverFeeStatus ->
  Maybe Int ->
  ServiceNames ->
  Bool ->
  m [DriverFee] -- remove maybe from merchantId later
findAllFeesInRangeWithStatusAndServiceName mbMerchantId merchantOperatingCityId startTime endTime status mbLimit serviceName enableCityBasedFeeSwitch =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId),
          Se.Is BeamDF.feeType $ Se.In [RECURRING_EXECUTION_INVOICE, RECURRING_INVOICE]
        ]
          <> [Se.Is BeamDF.merchantId $ Se.Eq $ getId (fromJust mbMerchantId) | isJust mbMerchantId]
          <> [Se.Is BeamDF.siblingFeeId $ Se.Eq Nothing | enableCityBasedFeeSwitch]
    ]
    (Se.Desc BeamDF.endTime)
    mbLimit
    Nothing

findFeeInRangeAndDriverIdAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  UTCTime ->
  UTCTime ->
  Id Person ->
  ServiceNames ->
  m [DriverFee]
findFeeInRangeAndDriverIdAndServiceName startTime endTime driverId serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.platformFee $ Se.GreaterThanOrEq 1.0,
          Se.Is BeamDF.driverId $ Se.Eq (getId driverId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.feeType $ Se.Not $ Se.In [MANDATE_REGISTRATION, PAYOUT_REGISTRATION, ONE_TIME_SECURITY_DEPOSIT]
        ]
    ]

findWindowsWithStatusAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  UTCTime ->
  UTCTime ->
  Maybe DriverFeeStatus ->
  Int ->
  Int ->
  ServiceNames ->
  m [DriverFee]
findWindowsWithStatusAndServiceName (Id driverId) from to mbStatus limitVal offsetVal serviceName =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.endTime $ Se.GreaterThanOrEq from,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq to,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
          <> [Se.Is BeamDF.status $ Se.Eq $ fromJust mbStatus | isJust mbStatus]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just limitVal)
    (Just offsetVal)

findWindowsAndServiceNameWithFeeTypeAndLimitAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  FeeType ->
  Int ->
  ServiceNames ->
  m [DriverFee]
findWindowsAndServiceNameWithFeeTypeAndLimitAndServiceName merchantId merchantOpCityId from to feeType limit serviceName =
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq from,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq to,
          Se.Is BeamDF.feeType $ Se.Eq feeType,
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId,
          Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId),
          Se.Is BeamDF.overlaySent $ Se.Eq False,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.platformFee $ Se.Not (Se.Eq 0.0),
          Se.Is BeamDF.cgst $ Se.Not (Se.Eq 0.0),
          Se.Is BeamDF.sgst $ Se.Not (Se.Eq 0.0),
          Se.Is BeamDF.siblingFeeId $ Se.Eq Nothing
        ]
    ]
    (Just limit)
    Nothing

findWindowsWithoutLimit :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> UTCTime -> UTCTime -> m [DriverFee]
findWindowsWithoutLimit (Id driverId) from to = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq from,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq to,
          Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.Not $ Se.In [INACTIVE, ONGOING]
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    Nothing
    Nothing

findDriverFeeInRangeWithNotifcationNotSentServiceNameAndStatus ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Int ->
  UTCTime ->
  UTCTime ->
  Int ->
  Domain.DriverFeeStatus ->
  ServiceNames ->
  m [DriverFee]
findDriverFeeInRangeWithNotifcationNotSentServiceNameAndStatus merchantId merchantOperatingCityId limit startTime endTime retryCount status serviceName = do
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE,
          Se.Is BeamDF.autopayPaymentStage $ Se.Eq (Just NOTIFICATION_SCHEDULED),
          Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId),
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Is BeamDF.notificationRetryCount $ Se.LessThanOrEq retryCount,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId
        ]
    ]
    (Just limit)
    Nothing

findDriverFeeInRangeWithOrderNotExecutedAndPendingByServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Int ->
  UTCTime ->
  UTCTime ->
  ServiceNames ->
  m [DriverFee]
findDriverFeeInRangeWithOrderNotExecutedAndPendingByServiceName merchantId merchantOperatingCityId limit startTime endTime serviceName = do
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE,
          Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING,
          Se.Is BeamDF.autopayPaymentStage $ Se.Eq (Just EXECUTION_SCHEDULED),
          Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId
        ]
    ]
    (Just limit)
    Nothing

findMaxBillNumberInRangeForServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  ServiceNames ->
  m [DriverFee]
findMaxBillNumberInRangeForServiceName merchantOperatingCityId startTime endTime serviceName =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId)
        ]
    ]
    (Se.Desc BeamDF.billNumber)
    (Just 1)
    Nothing

updateFee ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DriverFee ->
  Maybe HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  UTCTime ->
  Bool ->
  SRB.Booking ->
  Bool ->
  m ()
updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now isRideEnd _booking isSpecialZoneCharge = do
  driverFeeObject <- findById driverFeeId
  case driverFeeObject of
    Just df -> do
      let govtCharges' = df.govtCharges
      let platformFee' = df.platformFee.fee
      let cgst' = df.platformFee.cgst
      let sgst' = df.platformFee.sgst
      let totalEarnings = df.totalEarnings
      let numRides = df.numRides + if isRideEnd then 1 else 0
      let fare = fromMaybe 0.0 mbFare
          specialZoneRideCount' = df.specialZoneRideCount
          specialZoneAmount' = df.specialZoneAmount
          totalDriverFee = govtCharges + platformFee + cgst + sgst
      updateOneWithKV
        ( [ Se.Set BeamDF.govtCharges $ roundToIntegral $ govtCharges' + govtCharges,
            Se.Set BeamDF.govtChargesAmount $ Just $ govtCharges' + govtCharges,
            Se.Set BeamDF.platformFee $ platformFee' + platformFee,
            Se.Set BeamDF.cgst $ cgst' + cgst,
            Se.Set BeamDF.sgst $ sgst' + sgst,
            Se.Set BeamDF.totalEarnings $ roundToIntegral $ totalEarnings + fare,
            Se.Set BeamDF.totalEarningsAmount $ Just $ totalEarnings + fare,
            Se.Set BeamDF.numRides numRides,
            Se.Set BeamDF.updatedAt now
          ]
            <> [Se.Set BeamDF.specialZoneRideCount $ specialZoneRideCount' + 1 | isSpecialZoneCharge]
            <> [Se.Set BeamDF.specialZoneAmount $ specialZoneAmount' + totalDriverFee | isSpecialZoneCharge]
        )
        [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]
    Nothing -> pure ()

updateCancellationPenaltyAmount ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DriverFee ->
  HighPrecMoney ->
  UTCTime ->
  m ()
updateCancellationPenaltyAmount driverFeeId newAmount now = do
  updateOneWithKV
    [ Se.Set BeamDF.cancellationPenaltyAmount (Just newAmount),
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]

updateCancellationPenaltyAmountAndNumRides ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DriverFee ->
  HighPrecMoney ->
  Int ->
  UTCTime ->
  m ()
updateCancellationPenaltyAmountAndNumRides driverFeeId newAmount numRides now = do
  updateOneWithKV
    [ Se.Set BeamDF.cancellationPenaltyAmount (Just newAmount),
      Se.Set BeamDF.numRides numRides,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]

findOngoingCancellationPenaltyFeeByDriverIdAndServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Driver ->
  ServiceNames ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  UTCTime ->
  m (Maybe DriverFee)
findOngoingCancellationPenaltyFeeByDriverIdAndServiceName (Id driverId) serviceName merchantId merchantOperatingCityId now = do
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is BeamDF.driverId (Se.Eq driverId),
            Se.Is BeamDF.feeType (Se.Eq CANCELLATION_PENALTY),
            Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
            Se.Is BeamDF.status (Se.Eq ONGOING),
            Se.Is BeamDF.merchantId (Se.Eq merchantId.getId),
            Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId),
            Se.Is BeamDF.startTime $ Se.LessThanOrEq now,
            Se.Is BeamDF.endTime $ Se.GreaterThanOrEq now
          ]
        )
    ]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

resetFee ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DriverFee ->
  HighPrecMoney ->
  Domain.PlatformFee ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  UTCTime ->
  m ()
resetFee driverFeeId govtCharges platformFee mbFeeWithoutDiscount mbAmountPaidByCoin now = do
  updateOneWithKV
    ( [ Se.Set BeamDF.govtCharges $ roundToIntegral govtCharges,
        Se.Set BeamDF.govtChargesAmount $ Just govtCharges,
        Se.Set BeamDF.platformFee platformFee.fee,
        Se.Set BeamDF.cgst platformFee.cgst,
        Se.Set BeamDF.sgst platformFee.sgst,
        Se.Set BeamDF.updatedAt now
      ]
        <> [Se.Set BeamDF.feeWithoutDiscount mbFeeWithoutDiscount | isJust mbFeeWithoutDiscount]
        <> [Se.Set BeamDF.amountPaidByCoin mbAmountPaidByCoin | isJust mbAmountPaidByCoin]
    )
    [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]

updateRefundData ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DriverFee ->
  RefundInfo ->
  m ()
updateRefundData driverFeeId RefundInfo {..} = do
  updateOneWithKV
    ( [Se.Set BeamDF.refundedBy refundedBy | isJust refundedBy]
        <> [Se.Set BeamDF.refundEntityId refundEntityId | isJust refundEntityId]
        <> [Se.Set BeamDF.refundedAt refundedAt | isJust refundedAt]
        <> [Se.Set BeamDF.status status' | Just status' <- [status]]
        <> [Se.Set BeamDF.refundedAmount refundedAmount | isJust refundedAmount]
    )
    [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]

updateAutopayPaymentStageByIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Domain.AutopayPaymentStage -> [Id DriverFee] -> m ()
updateAutopayPaymentStageByIds autopayPaymentStage driverFeeIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.autopayPaymentStage autopayPaymentStage,
      Se.Set BeamDF.stageUpdatedAt (Just now)
    ]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

updateAutopayPaymentStageAndRetryCountByIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Domain.AutopayPaymentStage -> Int -> [Id DriverFee] -> m ()
updateAutopayPaymentStageAndRetryCountByIds autopayPaymentStage retryCount driverFeeIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.autopayPaymentStage autopayPaymentStage,
      Se.Set BeamDF.stageUpdatedAt (Just now),
      Se.Set BeamDF.notificationRetryCount retryCount
    ]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

--- note :- bad debt recovery date set in fork pls remeber to add fork in all places with driver fee status update in future----
updateStatusByIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverFeeStatus -> [Id DriverFee] -> UTCTime -> m ()
updateStatusByIds status driverFeeIds now = do
  case status of
    CLEARED -> do
      updateWithKV
        [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now, Se.Set BeamDF.collectedAt (Just now)]
        [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]
    _ -> do
      updateWithKV
        ( [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
            <> [Se.Set BeamDF.badDebtDeclarationDate $ Just now | status `elem` [EXEMPTED, INACTIVE]]
        )
        [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]
  fork "set bad recovery date" $ do updateBadDebtRecoveryDate status driverFeeIds

updateFeeTypeByIds :: (MonadFlow m, EsqDBFlow m r) => FeeType -> [Id DriverFee] -> UTCTime -> m ()
updateFeeTypeByIds feeType driverFeeIds now =
  updateWithKV
    [ Se.Set BeamDF.feeType feeType,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

updateDriverFeeOverlayScheduledByServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id Person] ->
  Bool ->
  UTCTime ->
  UTCTime ->
  ServiceNames ->
  m ()
updateDriverFeeOverlayScheduledByServiceName driverIds val from to serviceName =
  updateWithKV
    [ Se.Set BeamDF.overlaySent val
    ]
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.In (getId <$> driverIds),
          Se.Is BeamDF.startTime $ Se.GreaterThanOrEq from,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq to,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.feeType $ Se.Not $ Se.In [MANDATE_REGISTRATION, PAYOUT_REGISTRATION, ONE_TIME_SECURITY_DEPOSIT]
        ]
    ]

updateToManualFeeByDriverFeeIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DriverFee] -> m ()
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

findAllByStatusAndDriverIdWithServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  [Domain.DriverFeeStatus] ->
  Maybe [Id DriverFee] ->
  ServiceNames ->
  m [DriverFee]
findAllByStatusAndDriverIdWithServiceName (Id driverId) driverFeeStatus mbDriverFeeIds serviceName = do
  findAllWithKV
    [ Se.And $
        [ Se.Is BeamDF.feeType $ Se.In [RECURRING_INVOICE],
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.status $ Se.In driverFeeStatus,
          Se.Is BeamDF.driverId $ Se.Eq driverId
        ]
          <> maybe [] (\driverFeeIds -> [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]) mbDriverFeeIds
    ]

findAllDriverFeesRequiredToMovedIntoBadDebt :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> TransporterConfig -> m [DriverFee]
findAllDriverFeesRequiredToMovedIntoBadDebt merchantId transporterConfig = do
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let badDebtTimeThreshold = fromIntegral transporterConfig.badDebtTimeThreshold
      lastDayOfPreviousMonth = getLastDayOfMonth now
      range = UTCTime (addDays (-1 * badDebtTimeThreshold) lastDayOfPreviousMonth) (23 * 3600 + 59 * 60)
      limit = transporterConfig.badDebtBatchSize
  findAllWithOptionsKV'
    [ Se.Or
        [ Se.And
            [ Se.Is BeamDF.status $ Se.In [PAYMENT_OVERDUE, PAYMENT_PENDING],
              Se.Is BeamDF.badDebtDeclarationDate $ Se.Eq Nothing,
              Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId,
              Se.Is BeamDF.startTime $ Se.LessThanOrEq range
            ],
          Se.And
            [ Se.Is BeamDF.status $ Se.In [EXEMPTED, INACTIVE],
              Se.Is BeamDF.badDebtDeclarationDate $ Se.Eq Nothing,
              Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId
            ]
        ]
    ]
    (Just limit)
    Nothing

findAllByDriverFeeIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DriverFee] -> m [DriverFee]
findAllByDriverFeeIds driverFeeIds = do
  findAllWithKV
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

findLatestByFeeTypeAndStatusWithServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Domain.FeeType ->
  [Domain.DriverFeeStatus] ->
  Id Person ->
  ServiceNames ->
  m (Maybe DriverFee)
findLatestByFeeTypeAndStatusWithServiceName feeType status driverId serviceName = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.Eq feeType,
          Se.Is BeamDF.status $ Se.In status,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.driverId $ Se.Eq driverId.getId
        ]
    ]
    (Se.Desc BeamDF.updatedAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findLatestByFeeTypeAndStatusWithTotalEarnings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Domain.FeeType ->
  [Domain.DriverFeeStatus] ->
  Id Person ->
  HighPrecMoney ->
  m (Maybe DriverFee)
findLatestByFeeTypeAndStatusWithTotalEarnings feeType status driverId totalEarnings = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.Eq feeType,
          Se.Is BeamDF.status $ Se.In status,
          Se.Is BeamDF.totalEarningsAmount $ Se.Eq (Just totalEarnings),
          Se.Is BeamDF.driverId $ Se.Eq driverId.getId
        ]
    ]
    (Se.Desc BeamDF.updatedAt)
    (Just 1)
    Nothing
    <&> listToMaybe

-- TODO : Merge relevant queries
findAllByTimeMerchantAndStatusWithServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Merchant ->
  UTCTime ->
  UTCTime ->
  [Domain.DriverFeeStatus] ->
  ServiceNames ->
  m [DriverFee]
findAllByTimeMerchantAndStatusWithServiceName (Id merchantId) startTime endTime status serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.merchantId $ Se.Eq merchantId,
          Se.Is BeamDF.endTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.In status
        ]
    ]

--- note :- bad debt recovery date set in fork pls remeber to add fork in all places with driver fee status update in future----
updateStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverFeeStatus -> Id DriverFee -> UTCTime -> m ()
updateStatus status (Id driverFeeId) now = do
  updateOneWithKV
    ( [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
        <> [Se.Set BeamDF.badDebtDeclarationDate $ Just now | status == EXEMPTED]
    )
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]
  fork "set bad recovery date" $ do updateBadDebtRecoveryDate status [Id driverFeeId]

updateAutoPayToManual :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DriverFee -> m ()
updateAutoPayToManual driverFeeId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.feeType RECURRING_INVOICE,
      Se.Set BeamDF.status PAYMENT_OVERDUE,
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.id (Se.Eq driverFeeId.getId),
          Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE
        ]
    ]

updateDriverFeeToManual :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DriverFee -> m ()
updateDriverFeeToManual driverFeeId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.feeType RECURRING_INVOICE,
      Se.Set BeamDF.status PAYMENT_OVERDUE,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

updateManualToAutoPay :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DriverFee -> m ()
updateManualToAutoPay driverFeeId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.feeType RECURRING_EXECUTION_INVOICE,
      Se.Set BeamDF.status PAYMENT_PENDING,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

--- note :- bad debt recovery date set in fork pls remeber to add fork in all places with driver fee status update in future----
updateRegisterationFeeStatusByDriverIdForServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DriverFeeStatus ->
  Id Person ->
  ServiceNames ->
  m ()
updateRegisterationFeeStatusByDriverIdForServiceName status (Id driverId) serviceName = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
        <> [Se.Set BeamDF.badDebtDeclarationDate $ Just now | status `elem` [EXEMPTED, INACTIVE]]
    )
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.feeType (Se.Eq MANDATE_REGISTRATION),
          Se.Is BeamDF.status (Se.Eq PAYMENT_PENDING),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

--- note :- bad debt recovery date set in fork pls remeber to add fork in all places with driver fee status update in future----
updateCollectedPaymentStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverFeeStatus -> Maybe Text -> UTCTime -> Maybe Text -> Id DriverFee -> m ()
updateCollectedPaymentStatus status volunteerId now mbVendorId (Id driverFeeId) = do
  updateOneWithKV
    ( [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now, Se.Set BeamDF.collectedBy volunteerId, Se.Set BeamDF.collectedAt (Just now), Se.Set BeamDF.collectedAtVendorId mbVendorId]
        <> [Se.Set BeamDF.badDebtDeclarationDate $ Just now | status `elem` [EXEMPTED, INACTIVE]]
    )
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]
  fork "set bad recovery date" $ do updateBadDebtRecoveryDate status [Id driverFeeId]

updateAllExecutionPendingToManualOverdueByDriverIdForServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> ServiceNames -> m ()
updateAllExecutionPendingToManualOverdueByDriverIdForServiceName driverId serviceName = do
  updateWithKV
    [Se.Set BeamDF.feeType RECURRING_INVOICE, Se.Set BeamDF.status PAYMENT_OVERDUE]
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId.getId),
          Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE,
          Se.Is BeamDF.autopayPaymentStage $ Se.Not $ Se.Eq (Just EXECUTION_ATTEMPTING)
        ]
    ]

updateBadDebtDateAllDriverFeeIds :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> [Id DriverFee] -> TransporterConfig -> m ()
updateBadDebtDateAllDriverFeeIds merchantId driverFeeIds transporterConfig = do
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let lastDayOfPreviousMonth = getLastDayOfMonth now
      dateMovedToBadDebt = UTCTime lastDayOfPreviousMonth (23 * 3600 + 59 * 60)
  updateWithKV
    [Se.Set BeamDF.badDebtDeclarationDate (Just dateMovedToBadDebt)]
    [ Se.And
        [ Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds),
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId
        ]
    ]

updateBadDebtRecoveryDate :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DriverFeeStatus -> [Id DriverFee] -> m ()
updateBadDebtRecoveryDate status driverFeeIds = do
  utcNow <- getCurrentTime
  when (status `elem` [COLLECTED_CASH, CLEARED]) $ do
    updateWithKV
      [Se.Set BeamDF.badDebtRecoveryDate $ Just utcNow]
      [ Se.And
          [ Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds),
            Se.Is BeamDF.badDebtDeclarationDate $ Se.Not (Se.Eq Nothing),
            Se.Is BeamDF.badDebtRecoveryDate $ Se.Eq Nothing
          ]
      ]

getLastDayOfMonth :: UTCTime -> Day
getLastDayOfMonth now = do
  let (year, month, _) = toGregorian (utctDay now)
  addDays (-1) $ fromGregorian year month 1

findAllChildsOFDriverFee ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  DriverFeeStatus ->
  ServiceNames ->
  [Id DriverFee] ->
  Bool ->
  m [DriverFee]
findAllChildsOFDriverFee merchantOperatingCityId startTime endTime status serviceName driverFeeIds enableCityBasedFeeSwitch = do
  findAllWithKV
    [ Se.And
        ( [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
            Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
            Se.Is BeamDF.status $ Se.Eq status,
            Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
            Se.Is BeamDF.feeType $ Se.In [RECURRING_EXECUTION_INVOICE, RECURRING_INVOICE],
            Se.Is BeamDF.siblingFeeId $ Se.In (map Just $ getId <$> driverFeeIds)
          ]
            <> [Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId) | enableCityBasedFeeSwitch]
        )
    ]

updateDfeeByOperatingCityAndVehicleCategory ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Id Person ->
  ServiceNames ->
  DriverFeeStatus ->
  UTCTime ->
  UTCTime ->
  DVC.VehicleCategory ->
  Text ->
  Bool ->
  m ()
updateDfeeByOperatingCityAndVehicleCategory merchantOperatingCityId driverId serviceName status from to vehicleCategory planId isSubscriptionEnabledAtCategoryLevel = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.planId (Just planId),
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        ( [ Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
            Se.Is BeamDF.status $ Se.Eq status,
            Se.Is BeamDF.startTime $ Se.GreaterThanOrEq from,
            Se.Is BeamDF.endTime $ Se.LessThanOrEq to,
            Se.Is BeamDF.vehicleCategory $ Se.Eq (Just vehicleCategory),
            Se.Is BeamDF.driverId $ Se.Eq (getId driverId)
          ]
            <> [Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId) | isSubscriptionEnabledAtCategoryLevel]
        )
    ]

updateHasSiblingInDriverFee :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DriverFee -> m ()
updateHasSiblingInDriverFee driverFeeId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.hasSibling (Just True),
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

-- | Find all PAYMENT_PENDING cancellation penalties for a driver
findPendingCancellationPenaltiesForDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Driver ->
  ServiceNames ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [DriverFee]
findPendingCancellationPenaltiesForDriver (Id driverId) serviceName merchantId merchantOperatingCityId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.feeType $ Se.Eq CANCELLATION_PENALTY,
          Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId,
          Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId)
        ]
    ]

-- | Update status to ADDED_TO_INVOICE and link to parent DriverFee
updateStatusAndAddedToFeeId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Domain.DriverFeeStatus ->
  Maybe (Id DriverFee) ->
  [Id DriverFee] ->
  UTCTime ->
  m ()
updateStatusAndAddedToFeeId newStatus addedToFeeId driverFeeIds now =
  updateWithKV
    [ Se.Set BeamDF.status newStatus,
      Se.Set BeamDF.addedToFeeId ((.getId) <$> addedToFeeId),
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.In ((.getId) <$> driverFeeIds))]

-- | Find all cancellation penalties that were added to a specific DriverFee
findAllByAddedToFeeId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DriverFee ->
  m [DriverFee]
findAllByAddedToFeeId driverFeeId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.addedToFeeId $ Se.Eq (Just driverFeeId.getId),
          Se.Is BeamDF.feeType $ Se.Eq CANCELLATION_PENALTY
        ]
    ]

findUnbilledCancellationPenaltiesForDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Driver ->
  ServiceNames ->
  m [DriverFee]
findUnbilledCancellationPenaltiesForDriver (Id driverId) serviceName =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.feeType $ Se.Eq CANCELLATION_PENALTY,
          Se.Is BeamDF.status $ Se.In [ONGOING, IN_DISPUTE_WINDOW, PAYMENT_PENDING],
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]

findSubscriptionFeesWithCancellationPenalties ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Driver ->
  ServiceNames ->
  m [DriverFee]
findSubscriptionFeesWithCancellationPenalties (Id driverId) serviceName =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.feeType $ Se.In [RECURRING_INVOICE, RECURRING_EXECUTION_INVOICE],
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.cancellationPenaltyAmount $ Se.Not $ Se.Eq Nothing
        ]
    ]

findOriginalCancellationPenaltiesForSubscriptionFee ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id DriverFee] ->
  m [DriverFee]
findOriginalCancellationPenaltiesForSubscriptionFee subscriptionFeeIds =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.addedToFeeId $ Se.In (map pure $ (.getId) <$> subscriptionFeeIds),
          Se.Is BeamDF.feeType $ Se.Eq CANCELLATION_PENALTY
        ]
    ]

findParentRecurringExecutionBySplitOfDriverFeeIds ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id DriverFee] ->
  m [DriverFee]
findParentRecurringExecutionBySplitOfDriverFeeIds splitOfDriverFeeIds =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.id $ Se.In ((.getId) <$> splitOfDriverFeeIds),
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_EXECUTION_INVOICE
        ]
    ]

moveCancellationPenaltiesToIndisputeWindow ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  ServiceNames ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m ()
moveCancellationPenaltiesToIndisputeWindow serviceName merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.status IN_DISPUTE_WINDOW,
      Se.Set BeamDF.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.Eq CANCELLATION_PENALTY,
          Se.Is BeamDF.status $ Se.Eq ONGOING,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId,
          Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId),
          Se.Is BeamDF.endTime $ Se.LessThanOrEq now
        ]
    ]

moveCancellationPenaltyToPaymentPending ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DriverFee ->
  HighPrecMoney ->
  m ()
moveCancellationPenaltyToPaymentPending driverFeeId amountWithGst = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.status PAYMENT_PENDING,
      Se.Set BeamDF.updatedAt now,
      Se.Set BeamDF.cancellationPenaltyAmount (Just amountWithGst)
    ]
    [ Se.Is BeamDF.id $ Se.Eq driverFeeId.getId
    ]

findAllCancellationPenaltiesInDisputeWindow ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  ServiceNames ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [DriverFee]
findAllCancellationPenaltiesInDisputeWindow serviceName merchantId merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.Eq CANCELLATION_PENALTY,
          Se.Is BeamDF.status $ Se.Eq IN_DISPUTE_WINDOW,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId,
          Se.Is BeamDF.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId)
        ]
    ]
