{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.DriverFee where

import Control.Lens ((^?))
import Data.List ((\\))
import qualified Data.List as DL
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.Invoice as Domain
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.MerchantOperatingCity as MOC
import Domain.Types.Person (Person)
import Domain.Types.Plan (Plan, ServiceNames (..))
import qualified Domain.Types.Plan as Plan
import Domain.Types.TransporterConfig as TC
import EulerHS.Prelude hiding (id, state, (^?))
import GHC.Float (double2Int)
import GHC.Records.Extra
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common hiding (UTCTime)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.Invoice as QINV
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

data DriverFeeByInvoice = DriverFeeByInvoice
  { invoiceId :: Id INV.Invoice,
    date :: Day,
    govtCharges :: HighPrecMoney,
    platformFee :: PlatformFee,
    numRides :: Int,
    payBy :: UTCTime,
    totalEarnings :: HighPrecMoney,
    totalFee :: HighPrecMoney,
    startTime :: UTCTime,
    endTime :: UTCTime,
    status :: DDF.DriverFeeStatus,
    currency :: Currency
  }

data PlatformFee = PlatformFee
  { fee :: HighPrecMoney,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney,
    currency :: Currency
  }

groupDriverFeeByInvoices :: (EsqDBReplicaFlow m r, EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Currency -> [DDF.DriverFee] -> m [DriverFeeByInvoice]
groupDriverFeeByInvoices currency driverFees_ = do
  let pendingFees = filter (\df -> elem df.status [DDF.PAYMENT_PENDING, DDF.PAYMENT_OVERDUE]) driverFees_

  pendingFeeInvoiceId <- getInvoiceIdForPendingFees pendingFees
  uniqueInvoiceIds <- getUniqueInvoiceIds driverFees_ pendingFeeInvoiceId -- except this pendingFeeInvoiceId
  pendingFeeInvoiceResp <- buildDriverFeeByInvoice driverFees_ (Just DDF.PAYMENT_PENDING) pendingFeeInvoiceId
  otherInvoiceResp <- mapM (buildDriverFeeByInvoice driverFees_ Nothing) uniqueInvoiceIds

  return ([pendingFeeInvoiceResp | pendingFeeInvoiceResp.totalFee /= 0] <> otherInvoiceResp)
  where
    getUniqueInvoiceIds :: (EsqDBReplicaFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [DDF.DriverFee] -> Id INV.Invoice -> m [Id INV.Invoice]
    getUniqueInvoiceIds driverFees pendingFeeInvoiceId = do
      invoices <- (QINV.findValidByDriverFeeId . (.id)) `mapM` driverFees
      let uniqueInvoicesIds = map (.id) (mergeSortAndRemoveDuplicate invoices)
      return $ filter (pendingFeeInvoiceId /=) uniqueInvoicesIds

    getInvoiceIdForPendingFees :: (EsqDBReplicaFlow m r, EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [DDF.DriverFee] -> m (Id INV.Invoice)
    getInvoiceIdForPendingFees pendingFees = do
      invoices <- mapM (\fee -> runInReplica (QINV.findActiveManualInvoiceByFeeId fee.id Domain.MANUAL_INVOICE Domain.ACTIVE_INVOICE)) pendingFees
      let sortedInvoices = mergeSortAndRemoveDuplicate invoices
      let createNewInvoice = or (null <$> invoices)
      if createNewInvoice
        then do
          inactivateInvoices sortedInvoices
          insertInvoiceAgainstDriverFee pendingFees
        else do
          case sortedInvoices of
            [] -> insertInvoiceAgainstDriverFee pendingFees
            (invoice : rest) -> do
              inactivateInvoices rest
              return invoice.id

    inactivateInvoices :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [INV.Invoice] -> m ()
    inactivateInvoices = mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id))

    mergeSortAndRemoveDuplicate :: [[INV.Invoice]] -> [INV.Invoice]
    mergeSortAndRemoveDuplicate invoices = do
      let uniqueInvoices = DL.nubBy (\x y -> x.id == y.id) (concat invoices)
      sortOn (Down . (.createdAt)) uniqueInvoices

    insertInvoiceAgainstDriverFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [DDF.DriverFee] -> m (Id INV.Invoice)
    insertInvoiceAgainstDriverFee driverFees = do
      invoiceId <- generateGUID
      invoiceShortId <- generateShortId
      now <- getCurrentTime
      let invoices = mkInvoiceAgainstDriverFee invoiceId.getId invoiceShortId.getShortId now <$> driverFees
      QINV.createMany invoices
      return invoiceId

    mkInvoiceAgainstDriverFee id shortId now driverFee = do
      INV.Invoice
        { id = Id id,
          invoiceShortId = shortId,
          driverFeeId = driverFee.id,
          invoiceStatus = INV.ACTIVE_INVOICE,
          maxMandateAmount = Nothing,
          paymentMode = INV.MANUAL_INVOICE, -- critical point :- should we pass the manual here or hardcode ---,
          bankErrorCode = Nothing,
          bankErrorMessage = Nothing,
          bankErrorUpdatedAt = Nothing,
          driverId = driverFee.driverId,
          lastStatusCheckedAt = Nothing,
          updatedAt = now,
          merchantId = Just driverFee.merchantId,
          merchantOperatingCityId = driverFee.merchantOperatingCityId,
          serviceName = driverFee.serviceName,
          createdAt = now
        }

    buildDriverFeeByInvoice ::
      (EsqDBReplicaFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
      [DDF.DriverFee] ->
      Maybe DDF.DriverFeeStatus ->
      Id INV.Invoice ->
      m DriverFeeByInvoice
    buildDriverFeeByInvoice driverFees mStatus invoiceId = do
      invoices <- QINV.findById invoiceId
      now <- getCurrentTime
      let driverFeeIds = invoices <&> (.driverFeeId)
          invoiceDriverFees = DL.filter (\x -> x.id `elem` driverFeeIds) driverFees
          date = utctDay $ maybe now (.createdAt) (listToMaybe invoices)
          numRides = sum (invoiceDriverFees <&> (.numRides))
          payBy = DL.minimum (invoiceDriverFees <&> (.payBy))
          startTime = DL.minimum (invoiceDriverFees <&> (.startTime))
          endTime = DL.maximum (invoiceDriverFees <&> (.endTime))
          totalEarnings = sum (invoiceDriverFees <&> (.totalEarnings))
          govtCharges = sum (invoiceDriverFees <&> (.govtCharges))
          fee = sum (invoiceDriverFees <&> (.platformFee.fee))
          cgst = sum (invoiceDriverFees <&> (.platformFee.cgst))
          sgst = sum (invoiceDriverFees <&> (.platformFee.sgst))
          cancellationPenaltyTotal = sum (invoiceDriverFees <&> (fromMaybe 0 . (.cancellationPenaltyAmount)))
          platformFee = PlatformFee {..}
          totalFee = govtCharges + platformFee.fee + platformFee.cgst + platformFee.sgst + cancellationPenaltyTotal
          status =
            case mStatus of
              (Just status_) -> status_
              Nothing -> maybe DDF.INACTIVE (.status) (listToMaybe invoiceDriverFees)

      return $ DriverFeeByInvoice {..}

changeAutoPayFeesAndInvoicesForDriverFeesToManual :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DDF.DriverFee] -> [Id DDF.DriverFee] -> m ()
changeAutoPayFeesAndInvoicesForDriverFeesToManual alldriverFeeIdsInBatch validDriverFeeIds = do
  let driverFeeIdsToBeShiftedToManual = alldriverFeeIdsInBatch \\ validDriverFeeIds
  QDF.updateToManualFeeByDriverFeeIds driverFeeIdsToBeShiftedToManual
  QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE driverFeeIdsToBeShiftedToManual Nothing

roundToHalf :: Currency -> HighPrecMoney -> HighPrecMoney
roundToHalf INR x = fromInteger (round (x * 2)) / 2
roundToHalf _ x = x

calcNumRides :: DDF.DriverFee -> TC.TransporterConfig -> Int
calcNumRides driverFee transporterConfig =
  if transporterConfig.considerSpecialZoneRidesForPlanCharges
    then driverFee.numRides
    else driverFee.numRides - driverFee.specialZoneRideCount

calculatePlatformFeeAttr :: HighPrecMoney -> Plan -> (HighPrecMoney, HighPrecMoney, HighPrecMoney)
calculatePlatformFeeAttr totalFee plan = do
  let platformFee = totalFee / HighPrecMoney (toRational $ 1 + plan.cgstPercentage + plan.sgstPercentage) -- this should be changed to HighPrecMoney
      cgst = HighPrecMoney (toRational plan.cgstPercentage) * platformFee
      sgst = HighPrecMoney (toRational plan.sgstPercentage) * platformFee
  (platformFee, cgst, sgst)

setCoinToCashUsedAmount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DDF.DriverFee -> HighPrecMoney -> m ()
setCoinToCashUsedAmount driverFee totalFee = do
  coinAdjustedInSubscriptionKeyExists <- getCoinAdjustedInSubscriptionByDriverIdKey (cast driverFee.driverId)
  let integralTotalFee = double2Int $ realToFrac totalFee
  case coinAdjustedInSubscriptionKeyExists of
    Just _ -> void $ Hedis.withCrossAppRedis $ Hedis.incrby (mkCoinAdjustedInSubscriptionByDriverIdKey (cast driverFee.driverId)) (fromIntegral integralTotalFee)
    Nothing -> setCoinAdjustedInSubscriptionByDriverIdKey (cast driverFee.driverId) (fromIntegral integralTotalFee)

mkCoinAdjustedInSubscriptionByDriverIdKey :: Id Person -> Text
mkCoinAdjustedInSubscriptionByDriverIdKey driverId = "DriverCoinUsedInSubscription:DriverId:" <> driverId.getId

coinToCashProcessingLockKey :: Id Person -> Text
coinToCashProcessingLockKey (Id driverId) = "CoinToCash:Processing:DriverId" <> driverId

getCoinAdjustedInSubscriptionByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe Int)
getCoinAdjustedInSubscriptionByDriverIdKey driverId = Hedis.withCrossAppRedis $ Hedis.get (mkCoinAdjustedInSubscriptionByDriverIdKey driverId)

delCoinAdjustedInSubscriptionByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
delCoinAdjustedInSubscriptionByDriverIdKey driverId = Hedis.withCrossAppRedis $ Hedis.del (mkCoinAdjustedInSubscriptionByDriverIdKey driverId)

setCoinAdjustedInSubscriptionByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Integer -> m ()
setCoinAdjustedInSubscriptionByDriverIdKey driverId count = do
  _ <- Hedis.withCrossAppRedis $ Hedis.incrby (mkCoinAdjustedInSubscriptionByDriverIdKey driverId) count
  Hedis.withCrossAppRedis $ Hedis.expire (mkCoinAdjustedInSubscriptionByDriverIdKey driverId) 2592000 -- expire in 30 days

notificationSchedulerKey :: UTCTime -> UTCTime -> Id MOC.MerchantOperatingCity -> ServiceNames -> Text
notificationSchedulerKey startTime endTime merchantOpCityId serviceName = "NotificationScheduler:st:" <> show startTime <> ":et:" <> show endTime <> ":opCityid:" <> merchantOpCityId.getId <> ":serviceName:" <> show serviceName

isNotificationSchedulerRunningKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => UTCTime -> UTCTime -> Id MOC.MerchantOperatingCity -> ServiceNames -> m (Maybe Bool)
isNotificationSchedulerRunningKey startTime endTime merchantOpCityId serviceName = Hedis.withCrossAppRedis $ Hedis.get (notificationSchedulerKey startTime endTime merchantOpCityId serviceName)

setIsNotificationSchedulerRunningKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => UTCTime -> UTCTime -> Id MOC.MerchantOperatingCity -> ServiceNames -> Bool -> m ()
setIsNotificationSchedulerRunningKey startTime endTime merchantOpCityId serviceName isNotificationSchedulerRunning = do
  Hedis.withCrossAppRedis $ Hedis.setExp (notificationSchedulerKey startTime endTime merchantOpCityId serviceName) isNotificationSchedulerRunning (3600 * 24) -- one day expiry

createDriverFeeForServiceInSchedulerKey :: ServiceNames -> Id MOC.MerchantOperatingCity -> Text
createDriverFeeForServiceInSchedulerKey serviceName merchantOpCityId = "CreateDriverFeeFor:service:" <> show serviceName <> ":opCityid:" <> merchantOpCityId.getId

toCreateDriverFeeForService :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => ServiceNames -> Id MOC.MerchantOperatingCity -> m (Maybe Bool)
toCreateDriverFeeForService serviceName merchantOpCityId = Hedis.withCrossAppRedis $ Hedis.get (createDriverFeeForServiceInSchedulerKey serviceName merchantOpCityId)

setCreateDriverFeeForServiceInSchedulerKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => ServiceNames -> Id MOC.MerchantOperatingCity -> Bool -> m ()
setCreateDriverFeeForServiceInSchedulerKey serviceName merchantOpCityId createDriverFeeForService = do
  Hedis.withCrossAppRedis $ Hedis.setExp (createDriverFeeForServiceInSchedulerKey serviceName merchantOpCityId) createDriverFeeForService (3600 * 24) -- one day expiry

getStartTimeAndEndTimeRange :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MOC.MerchantOperatingCity -> Id Person -> Maybe TransporterConfig -> m (UTCTime, UTCTime)
getStartTimeAndEndTimeRange merchantOpCityId driverId transporterConfig = do
  now <- getCurrentTime
  transporterConfig' <- case transporterConfig of
    Just tc -> pure tc
    Nothing -> SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let potentialStart = addUTCTime transporterConfig'.driverPaymentCycleStartTime (UTCTime (utctDay now) (secondsToDiffTime 0))
      startTime = if now >= potentialStart then potentialStart else addUTCTime (-1 * transporterConfig'.driverPaymentCycleDuration) potentialStart
      endTime = addUTCTime transporterConfig'.driverPaymentCycleDuration startTime
  return (startTime, endTime)

mkCachedKeyTotalRidesByDriverId :: Id Person -> Text
mkCachedKeyTotalRidesByDriverId driverId = "TRIDES:DId:" <> driverId.getId

getPaymentModeAndVehicleCategoryKey :: Plan.Plan -> Text
getPaymentModeAndVehicleCategoryKey Plan.Plan {..} = show paymentMode <> "_" <> show vehicleCategory

jobDuplicationPreventionKey :: Text -> Text -> Text
jobDuplicationPreventionKey jobHash jobType = "JobDuplicationPreventionKey:Jt:-" <> jobType <> ":ParentJobDataHashed:-" <> jobHash

adjustDues :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [DDF.DriverFee] -> m ()
adjustDues dueDriverFees = do
  now <- getCurrentTime
  invoices <- mapM (\fee -> runInReplica (QINV.findActiveManualInvoiceByFeeId fee.id Domain.MANUAL_INVOICE Domain.ACTIVE_INVOICE)) dueDriverFees
  let invoicesToBeUpdated = mergeSortAndRemoveDuplicate invoices
  mapM_ (\inv -> QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE inv.id) invoicesToBeUpdated
  mapM_
    ( \dFee -> do
        invoice <- mkInvoice dFee
        QDF.updateStatus DDF.ONE_TIME_SECURITY_ADJUSTED dFee.id now
        QINV.create invoice
    )
    dueDriverFees
  where
    mergeSortAndRemoveDuplicate :: [[INV.Invoice]] -> [INV.Invoice]
    mergeSortAndRemoveDuplicate invoices = do
      let uniqueInvoices = DL.nubBy (\x y -> x.id == y.id) (concat invoices)
      sortOn (Down . (.createdAt)) uniqueInvoices
    mkInvoice driverFee = do
      id <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      return $
        INV.Invoice
          { id = Id id,
            invoiceShortId = shortId.getShortId,
            driverFeeId = driverFee.id,
            invoiceStatus = INV.SUCCESS,
            driverId = driverFee.driverId,
            maxMandateAmount = Nothing,
            paymentMode = INV.ONE_TIME_SECURITY_ADJUSTED_INVOICE,
            bankErrorCode = Nothing,
            bankErrorMessage = Nothing,
            bankErrorUpdatedAt = Nothing,
            lastStatusCheckedAt = Nothing,
            serviceName = driverFee.serviceName,
            merchantId = Just driverFee.merchantId,
            merchantOperatingCityId = driverFee.merchantOperatingCityId,
            updatedAt = now,
            createdAt = now
          }
