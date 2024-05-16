{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverFeeExtra where

import Data.Time (Day, UTCTime (UTCTime, utctDay), addDays, fromGregorian, toGregorian)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverFee
import qualified Domain.Types.DriverFee as Domain
import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Domain.Types.Person
import Domain.Types.Plan as DPlan
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (Currency (INR), HighPrecMoney (..), mkAmountWithDefault)
import Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Utils.Common (KvDbFlow, fork, fromMaybeM, getLocalCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFee as BeamDF
import Storage.Queries.OrphanInstances.DriverFee
import qualified Storage.Queries.Person as QP
import Tools.Error

-- Extra code goes here --

findById :: KvDbFlow m r => Id DriverFee -> m (Maybe DriverFee)
findById (Id driverFeeId) = findOneWithKV [Se.Is BeamDF.id $ Se.Eq driverFeeId]

findPendingFeesByDriverFeeId :: KvDbFlow m r => Id DriverFee -> m (Maybe DriverFee)
findPendingFeesByDriverFeeId (Id driverFeeId) =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.id $ Se.Eq driverFeeId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE]
        ]
    ]

findPendingFeesByDriverIdAndServiceName :: KvDbFlow m r => Id Driver -> ServiceNames -> m [DriverFee]
findPendingFeesByDriverIdAndServiceName (Id driverId) serviceName =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

findFeeByDriverIdAndServiceNameInRange ::
  KvDbFlow m r =>
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
          Se.Is BeamDF.feeType $ Se.Not (Se.Eq MANDATE_REGISTRATION)
        ]
    ]

findLatestFeeByDriverIdAndServiceName :: KvDbFlow m r => Id Driver -> ServiceNames -> m (Maybe DriverFee)
findLatestFeeByDriverIdAndServiceName (Id driverId) serviceName =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId (Se.Eq driverId),
          Se.Is BeamDF.feeType $ Se.Not (Se.Eq MANDATE_REGISTRATION),
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.status (Se.Eq ONGOING)
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findLatestRegisterationFeeByDriverIdAndServiceName :: KvDbFlow m r => Id Driver -> ServiceNames -> m (Maybe DriverFee)
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

findOldestFeeByStatusAndServiceName :: KvDbFlow m r => Id Driver -> DriverFeeStatus -> ServiceNames -> m (Maybe DriverFee)
findOldestFeeByStatusAndServiceName (Id driverId) status serviceName =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.status $ Se.Eq status
        ]
    ]
    (Se.Asc BeamDF.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

findFeesInRangeWithStatusAndServiceName ::
  KvDbFlow m r =>
  Maybe (Id Merchant) ->
  UTCTime ->
  UTCTime ->
  DriverFeeStatus ->
  Maybe Int ->
  ServiceNames ->
  m [DriverFee] -- remove maybe from merchantId later
findFeesInRangeWithStatusAndServiceName mbMerchantId startTime endTime status mbLimit serviceName =
  findAllWithOptionsKV'
    [ Se.And $
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.status $ Se.Eq status,
          Se.Or [Se.Is BeamDF.status (Se.Eq ONGOING), Se.Is BeamDF.payBy (Se.LessThanOrEq endTime)],
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
          <> [Se.Is BeamDF.merchantId $ Se.Eq $ getId (fromJust mbMerchantId) | isJust mbMerchantId]
    ]
    mbLimit
    Nothing

findAllFeesInRangeWithStatusAndServiceName ::
  KvDbFlow m r =>
  Maybe (Id Merchant) ->
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  DriverFeeStatus ->
  Maybe Int ->
  ServiceNames ->
  m [DriverFee] -- remove maybe from merchantId later
findAllFeesInRangeWithStatusAndServiceName mbMerchantId merchantOperatingCityId startTime endTime status mbLimit serviceName =
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
    ]
    (Se.Desc BeamDF.endTime)
    mbLimit
    Nothing

findPendingPaymentByDriversAndServiceName :: KvDbFlow m r => [Id Person] -> ServiceNames -> m [DriverFee]
findPendingPaymentByDriversAndServiceName driverIds serviceName =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.driverId $ Se.In (getId <$> driverIds)
        ]
    ]

findFeeInRangeAndDriverIdAndServiceName ::
  KvDbFlow m r =>
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
          Se.Is BeamDF.feeType $ Se.Not (Se.Eq MANDATE_REGISTRATION)
        ]
    ]

findWindowsWithStatusAndServiceName ::
  KvDbFlow m r =>
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
  KvDbFlow m r =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  FeeType ->
  Int ->
  ServiceNames ->
  m [DriverFee]
findWindowsAndServiceNameWithFeeTypeAndLimitAndServiceName merchantId merchantOpCityId from to feeType limit serviceName =
  findAllWithOptionsKV
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
          Se.Is BeamDF.sgst $ Se.Not (Se.Eq 0.0)
        ]
    ]
    (Se.Asc BeamDF.createdAt)
    (Just limit)
    Nothing

findWindowsAndServiceName ::
  KvDbFlow m r =>
  Id Person ->
  UTCTime ->
  UTCTime ->
  Int ->
  Int ->
  ServiceNames ->
  m [DriverFee]
findWindowsAndServiceName (Id driverId) from to limitVal offsetVal serviceName =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.createdAt $ Se.GreaterThanOrEq from,
          Se.Is BeamDF.createdAt $ Se.LessThanOrEq to,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName)
        ]
    ]
    (Se.Desc BeamDF.createdAt)
    (Just limitVal)
    (Just offsetVal)

findWindowsWithoutLimit :: KvDbFlow m r => Id Person -> UTCTime -> UTCTime -> m [DriverFee]
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

findOngoingAfterEndTimeAndServiceName :: KvDbFlow m r => Id Person -> UTCTime -> ServiceNames -> m (Maybe DriverFee)
findOngoingAfterEndTimeAndServiceName (Id driverId) now serviceName =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.Eq ONGOING,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq now,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

findDriverFeeInRangeWithNotifcationNotSentServiceNameAndStatus ::
  KvDbFlow m r =>
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
  KvDbFlow m r =>
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

findDriverFeeInRangeEligibleForManualPaymentLinkByServiceName ::
  KvDbFlow m r =>
  Id Merchant ->
  Int ->
  UTCTime ->
  UTCTime ->
  ServiceNames ->
  m [DriverFee]
findDriverFeeInRangeEligibleForManualPaymentLinkByServiceName merchantId limit startTime endTime serviceName = do
  findAllWithOptionsKV'
    [ Se.And
        [ Se.Is BeamDF.startTime $ Se.GreaterThanOrEq startTime,
          Se.Is BeamDF.endTime $ Se.LessThanOrEq endTime,
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE,
          Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.merchantId $ Se.Eq merchantId.getId
        ]
    ]
    (Just limit)
    Nothing

findMaxBillNumberInRangeForServiceName ::
  KvDbFlow m r =>
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

findUnpaidAfterPayByWithServiceName :: KvDbFlow m r => Id Person -> UTCTime -> ServiceNames -> m (Maybe DriverFee)
findUnpaidAfterPayByWithServiceName (Id driverId) now serviceName =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDF.driverId $ Se.Eq driverId,
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.payBy $ Se.LessThanOrEq now,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE
        ]
    ]

updateFee ::
  KvDbFlow m r =>
  Id DriverFee ->
  Maybe HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  UTCTime ->
  Bool ->
  SRB.Booking ->
  m ()
updateFee driverFeeId mbFare govtCharges platformFee cgst sgst now isRideEnd booking = do
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
            <> [Se.Set BeamDF.specialZoneRideCount $ specialZoneRideCount' + 1 | toUpdateSpecialZoneMetricsInDriverFee]
            <> [Se.Set BeamDF.specialZoneAmount $ specialZoneAmount' + totalDriverFee | toUpdateSpecialZoneMetricsInDriverFee]
        )
        [Se.Is BeamDF.id (Se.Eq (getId driverFeeId))]
    Nothing -> pure ()
  where
    toUpdateSpecialZoneMetricsInDriverFee = DTC.isRideOtpBooking booking.tripCategory

resetFee ::
  KvDbFlow m r =>
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

updateAutopayPaymentStageByIds :: KvDbFlow m r => Maybe Domain.AutopayPaymentStage -> [Id DriverFee] -> m ()
updateAutopayPaymentStageByIds autopayPaymentStage driverFeeIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.autopayPaymentStage autopayPaymentStage,
      Se.Set BeamDF.stageUpdatedAt (Just now)
    ]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

updateAutopayPaymentStageAndRetryCountByIds :: KvDbFlow m r => Maybe Domain.AutopayPaymentStage -> Int -> [Id DriverFee] -> m ()
updateAutopayPaymentStageAndRetryCountByIds autopayPaymentStage retryCount driverFeeIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDF.autopayPaymentStage autopayPaymentStage,
      Se.Set BeamDF.stageUpdatedAt (Just now),
      Se.Set BeamDF.notificationRetryCount retryCount
    ]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

--- note :- bad debt recovery date set in fork pls remeber to add fork in all places with driver fee status update in future----
updateStatusByIds :: KvDbFlow m r => DriverFeeStatus -> [Id DriverFee] -> UTCTime -> m ()
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

updateFeeTypeByIds :: KvDbFlow m r => FeeType -> [Id DriverFee] -> UTCTime -> m ()
updateFeeTypeByIds feeType driverFeeIds now =
  updateWithKV
    [ Se.Set BeamDF.feeType feeType,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

updateDriverFeeOverlayScheduledByServiceName ::
  KvDbFlow m r =>
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
          Se.Is BeamDF.feeType $ Se.Not $ Se.Eq Domain.MANDATE_REGISTRATION
        ]
    ]

updateToManualFeeByDriverFeeIds :: KvDbFlow m r => [Id DriverFee] -> m ()
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
  KvDbFlow m r =>
  Id Person ->
  [Domain.DriverFeeStatus] ->
  ServiceNames ->
  m [DriverFee]
findAllByStatusAndDriverIdWithServiceName (Id driverId) driverFeeStatus serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.In [RECURRING_INVOICE],
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.status $ Se.In driverFeeStatus,
          Se.Is BeamDF.driverId $ Se.Eq driverId
        ]
    ]

findAllPendingAndDueDriverFeeByDriverIdForServiceName :: KvDbFlow m r => Id Person -> ServiceNames -> m [DriverFee]
findAllPendingAndDueDriverFeeByDriverIdForServiceName (Id driverId) serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.In [RECURRING_INVOICE, RECURRING_EXECUTION_INVOICE],
          Se.Is BeamDF.status $ Se.In [PAYMENT_PENDING, PAYMENT_OVERDUE],
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.driverId $ Se.Eq driverId
        ]
    ]

findAllOverdueDriverFeeByDriverIdForServiceName :: KvDbFlow m r => Id Person -> ServiceNames -> m [DriverFee]
findAllOverdueDriverFeeByDriverIdForServiceName (Id driverId) serviceName =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.Eq RECURRING_INVOICE,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.status $ Se.Eq PAYMENT_OVERDUE,
          Se.Is BeamDF.driverId $ Se.Eq driverId
        ]
    ]

findAllPendingRegistrationDriverFeeByDriverIdForServiceName ::
  KvDbFlow m r =>
  Id Person ->
  ServiceNames ->
  m [DriverFee]
findAllPendingRegistrationDriverFeeByDriverIdForServiceName (Id driverId) serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDF.feeType $ Se.Eq MANDATE_REGISTRATION,
          Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
          Se.Is BeamDF.status $ Se.Eq PAYMENT_PENDING,
          Se.Is BeamDF.driverId $ Se.Eq driverId
        ]
    ]

findAllDriverFeesRequiredToMovedIntoBadDebt :: KvDbFlow m r => Id Merchant -> TransporterConfig -> m [DriverFee]
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

-- add fee collection time later if req'd
findAllByVolunteerIds :: KvDbFlow m r => Id Merchant -> [Text] -> UTCTime -> UTCTime -> m [DriverFee]
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

findAllByStatusAndServiceName ::
  KvDbFlow m r =>
  Id Merchant ->
  DriverFeeStatus ->
  UTCTime ->
  UTCTime ->
  ServiceNames ->
  m [DriverFee]
findAllByStatusAndServiceName (Id merchantId) status from to serviceName = do
  findAllWithOptionsKV
    [ Se.Is BeamDF.merchantId $ Se.Eq merchantId,
      Se.Is BeamDF.collectedAt $ Se.GreaterThanOrEq (Just from),
      Se.Is BeamDF.serviceName $ Se.Eq (Just serviceName),
      Se.Is BeamDF.collectedAt $ Se.LessThanOrEq (Just to),
      Se.Is BeamDF.status $ Se.Eq status
    ]
    (Se.Desc BeamDF.updatedAt)
    Nothing
    Nothing

findAllByDriverFeeIds :: KvDbFlow m r => [Id DriverFee] -> m [DriverFee]
findAllByDriverFeeIds driverFeeIds = do
  findAllWithKV
    [Se.Is BeamDF.id $ Se.In (getId <$> driverFeeIds)]

findLatestByFeeTypeAndStatusWithServiceName ::
  KvDbFlow m r =>
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

-- TODO : Merge relevant queries
findAllByTimeMerchantAndStatusWithServiceName ::
  KvDbFlow m r =>
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
updateStatus :: KvDbFlow m r => DriverFeeStatus -> Id DriverFee -> UTCTime -> m ()
updateStatus status (Id driverFeeId) now = do
  updateOneWithKV
    ( [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now]
        <> [Se.Set BeamDF.badDebtDeclarationDate $ Just now | status == EXEMPTED]
    )
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]
  fork "set bad recovery date" $ do updateBadDebtRecoveryDate status [Id driverFeeId]

updateAutoPayToManual :: KvDbFlow m r => Id DriverFee -> m ()
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

updateDriverFeeToManual :: KvDbFlow m r => Id DriverFee -> m ()
updateDriverFeeToManual driverFeeId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDF.feeType RECURRING_INVOICE,
      Se.Set BeamDF.status PAYMENT_OVERDUE,
      Se.Set BeamDF.updatedAt now
    ]
    [Se.Is BeamDF.id (Se.Eq driverFeeId.getId)]

updateManualToAutoPay :: KvDbFlow m r => Id DriverFee -> m ()
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
  KvDbFlow m r =>
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
updateCollectedPaymentStatus :: KvDbFlow m r => DriverFeeStatus -> Maybe Text -> UTCTime -> Id DriverFee -> m ()
updateCollectedPaymentStatus status volunteerId now (Id driverFeeId) = do
  updateOneWithKV
    ( [Se.Set BeamDF.status status, Se.Set BeamDF.updatedAt now, Se.Set BeamDF.collectedBy volunteerId, Se.Set BeamDF.collectedAt (Just now)]
        <> [Se.Set BeamDF.badDebtDeclarationDate $ Just now | status `elem` [EXEMPTED, INACTIVE]]
    )
    [Se.Is BeamDF.id (Se.Eq driverFeeId)]
  fork "set bad recovery date" $ do updateBadDebtRecoveryDate status [Id driverFeeId]

updateAllExecutionPendingToManualOverdueByDriverIdForServiceName :: KvDbFlow m r => Id Person -> ServiceNames -> m ()
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

updateBadDebtDateAllDriverFeeIds :: KvDbFlow m r => Id Merchant -> [Id DriverFee] -> TransporterConfig -> m ()
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

updateBadDebtRecoveryDate :: KvDbFlow m r => DriverFeeStatus -> [Id DriverFee] -> m ()
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
