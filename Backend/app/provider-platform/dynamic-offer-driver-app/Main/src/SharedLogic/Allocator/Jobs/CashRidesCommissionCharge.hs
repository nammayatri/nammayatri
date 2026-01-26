module SharedLogic.Allocator.Jobs.CashRidesCommissionCharge
  ( cashRidesCommissionCharge,
    cashRidesCommissionChargeProcessingKey,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.CashRidesCommission as DCashRidesCommission
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Stripe.Types.Transfer as Transfer
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.TransferTransaction as DTransferTransaction
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import SharedLogic.Allocator (AllocatorJobType (..), CashRidesCommissionChargeJobData)
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.CashRidesCommission as QCashRidesCommission
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error

cashRidesCommissionCharge ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    JobCreator r m,
    HasJobInfoMap r,
    HasShortDurationRetryCfg r c
  ) =>
  Job 'CashRidesCommissionCharge ->
  m ExecutionResult
cashRidesCommissionCharge Job {id, scheduledAt, jobInfo = jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOperatingCityId = jobData.merchantOperatingCityId
      merchantId = jobData.merchantId
      nextSettlementTime = jobData.nextSettlementTime
      paymentMode = jobData.paymentMode
      lastSettlementTime = getLastSettlementTime nextSettlementTime
  -- FIXME currently limit offset is not used
  allRides <- QRide.findAllByCityStatusInstrumentModeAndTimeRange merchantOperatingCityId DRide.COMPLETED DMPM.Cash paymentMode lastSettlementTime nextSettlementTime
  let fleetOwnerIds = nub $ mapMaybe (.fleetOwnerId) allRides
  let driverIdsWithoutFleet = nub $ (.driverId) <$> filter (isNothing . (.fleetOwnerId)) allRides
  logInfo $
    "Cash Rides Commission Charge: "
      <> "; merchantOperatingCityId: "
      <> merchantOperatingCityId.getId
      <> "; paymentMode: "
      <> show paymentMode
      <> "; lastSettlementTime: "
      <> show lastSettlementTime
      <> "; nextSettlementTime: "
      <> show nextSettlementTime
      <> "; all rides: "
      <> show (length allRides)
      <> "; fleetOwnerIds: "
      <> show (length fleetOwnerIds)
      <> "; driverIdsWithoutFleet: "
      <> show (length driverIdsWithoutFleet)

  forM_ fleetOwnerIds $ \fleetOwnerId -> do
    let fleetRides = filter (\ride -> ride.fleetOwnerId == Just fleetOwnerId) allRides
        isFleetOwner = True
    cashRidesCommissionChargeForSinglePerson' jobData fleetRides fleetOwnerId isFleetOwner

  forM_ driverIdsWithoutFleet $ \driverIdWithoutFleet -> do
    let driverRides = filter (\ride -> isNothing ride.fleetOwnerId && ride.driverId == driverIdWithoutFleet) allRides
        isFleetOwner = False
    cashRidesCommissionChargeForSinglePerson' jobData driverRides driverIdWithoutFleet isFleetOwner

  if jobData.reSchedule
    then do
      logInfo $ "Cash Rides Commission Charge finished successfully. Reschedule: " <> show nextSettlementTime
      let updJobData =
            jobData{nextSettlementTime = getNextSettlementTime nextSettlementTime
                   }
      QAllJ.createJobByTime @_ @'CashRidesCommissionCharge (Just merchantId) (Just merchantOperatingCityId) (getNextJobTime scheduledAt) updJobData
    else do
      logInfo "Cash Rides Commission Charge finished successfully."
  pure Complete

getLastSettlementTime, getNextSettlementTime, getNextJobTime :: UTCTime -> UTCTime
getLastSettlementTime = addUTCTime (negate 604800) -- subtract 7 days
getNextSettlementTime = addUTCTime 604800 -- add 7 days
getNextJobTime = getNextSettlementTime

cashRidesCommissionChargeForSinglePerson' ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  CashRidesCommissionChargeJobData ->
  [DRide.Ride] ->
  Id DP.Person ->
  Bool ->
  m ()
cashRidesCommissionChargeForSinglePerson' jobData personRides personId isFleetOwner = do
  let personTxt = if isFleetOwner then "fleet owner" else "driver"
  Redis.whenWithLockRedis (cashRidesCommissionChargeProcessingKey personId) 60 $ do
    withTryCatch ("cashRidesCommissionCharge:" <> personId.getId) (cashRidesCommissionChargeForSinglePerson jobData personRides personId isFleetOwner) >>= \case
      Left e -> logError $ "Failed to charge cash ride commission for " <> personTxt <> ": " <> personId.getId <> "; err: " <> show e
      Right () -> pure ()

cashRidesCommissionChargeForSinglePerson ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  CashRidesCommissionChargeJobData ->
  [DRide.Ride] ->
  Id DP.Person ->
  Bool ->
  m ()
cashRidesCommissionChargeForSinglePerson jobData personRides personId isFleetOwner = do
  let merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId
      nextSettlementTime = jobData.nextSettlementTime
      paymentMode = jobData.paymentMode
      lastSettlementTime = getLastSettlementTime nextSettlementTime
  let personTxt = if isFleetOwner then "fleet owner" else "driver"

  -- avoid double charge section
  overlappingCashRidesCommissions <- QCashRidesCommission.findAllByPersonModeAndOverlappingTimeRange personId paymentMode lastSettlementTime nextSettlementTime
  unless (null overlappingCashRidesCommissions) $ do
    logWarning $
      "Found overlapped cash rides commissions: "
        <> T.intercalate "; " (overlappingCashRidesCommissions <&> \commission -> "lastSettlementTime: " <> show commission.lastSettlementTime <> "; nextSettlementTime" <> show commission.nextSettlementTime <> ": " <> show commission.numberOfRides)
  let commissionAlreadyCharged personRide overlappingCashRidesCommission = do
        isJust personRide.tripEndTime
          && personRide.tripEndTime >= Just overlappingCashRidesCommission.lastSettlementTime
          && personRide.tripEndTime < Just overlappingCashRidesCommission.nextSettlementTime
  let personRidesExcludeOverlapping = filter (not . flip any overlappingCashRidesCommissions . commissionAlreadyCharged) personRides
  let overlappedRidesFiltered = length personRides - length personRidesExcludeOverlapping

  let totalCommission = sum $ (\ride -> fromMaybe 0.0 ride.commission) <$> personRidesExcludeOverlapping
  case listToMaybe personRidesExcludeOverlapping of
    Nothing -> logError $ "Cash Rides Commission Charge did not found for " <> personTxt <> ": " <> personId.getId -- should not appear
    Just firstRide -> do
      let currency = firstRide.currency
      let totalCommissionPrice = mkPrice (Just currency) totalCommission

      logInfo $
        "Cash Rides Commission Charge for "
          <> personTxt
          <> ": "
          <> personId.getId
          <> "; total rides: "
          <> show (length personRidesExcludeOverlapping)
          <> "; total commission: "
          <> showPriceWithRounding totalCommissionPrice
          <> (if overlappedRidesFiltered /= 0 then "; overlapped rides filtered: " <> show overlappedRidesFiltered else "")

      if totalCommission > 0
        then do
          personBankAccount <- B.runInReplica $ QDBA.findByPrimaryKey personId >>= fromMaybeM (DriverBankAccountNotFound personId.getId)
          let personPaymentMode = fromMaybe DMPM.LIVE personBankAccount.paymentMode
          if paymentMode == personPaymentMode
            then do
              person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
              cashRidesCommission <- buildCashRidesCommission totalCommissionPrice person paymentMode (length personRidesExcludeOverlapping)
              let createTranferReq =
                    Payment.CreateTransferReq
                      { amount = totalCommission,
                        currency,
                        senderConnectedAccountId = personBankAccount.accountId,
                        destinationAccount = Payment.TransferPlatformAccount,
                        description = Just $ "Cash rides commission charge for " <> personTxt <> " for period: " <> show lastSettlementTime <> " - " <> show nextSettlementTime
                      }
              let entityId = cast @DCashRidesCommission.CashRidesCommission @DTransferTransaction.TransferEntity cashRidesCommission.id
              updCashRidesCommission <-
                SPayment.makeStripeTransfer merchantId merchantOperatingCityId personBankAccount.paymentMode DTransferTransaction.CASH_RIDES_COMMISSION entityId createTranferReq >>= \case
                  Left err -> do
                    logError $ "Failed to charge cash ride commission for " <> personTxt <> ": " <> personId.getId <> "; entityId: " <> entityId.getId <> "; err: " <> err
                    pure cashRidesCommission{status = Transfer.TRANSFER_FAILED}
                  Right resp -> do
                    pure cashRidesCommission{status = resp.status}
              QCashRidesCommission.create updCashRidesCommission
            else do
              logInfo $ "Wrong payment mode for " <> personTxt <> ": " <> personId.getId <> "; person payment mode: " <> show personPaymentMode <> "; skipping"
        else do
          logInfo $ "Cash Rides Commission Charge is empty for " <> personTxt <> ": " <> personId.getId <> "; totalComission: " <> showPriceWithRounding totalCommissionPrice
  where
    buildCashRidesCommission totalCommissionPrice person paymentMode numberOfRides = do
      id <- generateGUID
      now <- getCurrentTime
      pure
        DCashRidesCommission.CashRidesCommission
          { id,
            amount = totalCommissionPrice.amount,
            currency = totalCommissionPrice.currency,
            lastSettlementTime = getLastSettlementTime jobData.nextSettlementTime,
            nextSettlementTime = jobData.nextSettlementTime,
            numberOfRides,
            personId,
            personRole = person.role,
            status = Transfer.TRANSFER_PENDING,
            merchantId = jobData.merchantId,
            merchantOperatingCityId = jobData.merchantOperatingCityId,
            paymentMode,
            createdAt = now,
            updatedAt = now
          }

cashRidesCommissionChargeProcessingKey :: Kernel.Types.Id.Id DP.Person -> Text
cashRidesCommissionChargeProcessingKey personId = "CashRidesCommissionCharge:Processing:PersonId" <> personId.getId
