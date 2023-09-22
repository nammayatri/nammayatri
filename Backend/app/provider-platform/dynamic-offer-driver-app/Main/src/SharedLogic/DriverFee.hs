{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverFee where

import Data.List ((\\))
import qualified Data.List as DL
import Data.Time (Day, UTCTime (utctDay))
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.Invoice as INV
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.Invoice as QINV

data DriverFeeByInvoice = DriverFeeByInvoice
  { invoiceId :: Id INV.Invoice,
    date :: Day,
    govtCharges :: HighPrecMoney,
    platformFee :: PlatformFee,
    numRides :: Int,
    payBy :: UTCTime,
    totalEarnings :: Money,
    totalFee :: Money,
    startTime :: UTCTime,
    endTime :: UTCTime,
    status :: DDF.DriverFeeStatus
  }

data PlatformFee = PlatformFee
  { fee :: HighPrecMoney,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney
  }

groupDriverFeeByInvoices :: (EsqDBReplicaFlow m r, EsqDBFlow m r, MonadFlow m) => [DDF.DriverFee] -> m [DriverFeeByInvoice]
groupDriverFeeByInvoices driverFees_ = do
  let pendingFees = filter (\df -> elem df.status [DDF.PAYMENT_PENDING, DDF.PAYMENT_OVERDUE]) driverFees_

  pendingFeeInvoiceId <- getInvoiceIdForPendingFees pendingFees
  uniqueInvoiceIds <- getUniqueInvoiceIds driverFees_ pendingFeeInvoiceId -- except this pendingFeeInvoiceId
  pendingFeeInvoiceResp <- buildDriverFeeByInvoice driverFees_ (Just DDF.PAYMENT_PENDING) pendingFeeInvoiceId
  otherInvoiceResp <- mapM (buildDriverFeeByInvoice driverFees_ Nothing) uniqueInvoiceIds

  return ([pendingFeeInvoiceResp | pendingFeeInvoiceResp.totalFee /= 0] <> otherInvoiceResp)
  where
    getUniqueInvoiceIds :: (EsqDBReplicaFlow m r, MonadFlow m) => [DDF.DriverFee] -> Id INV.Invoice -> m [Id INV.Invoice]
    getUniqueInvoiceIds driverFees pendingFeeInvoiceId = do
      invoices <- (QINV.findValidByDriverFeeId . (.id)) `mapM` driverFees
      let uniqueInvoicesIds = map (.id) (mergeSortAndRemoveDuplicate invoices)
      return $ filter (pendingFeeInvoiceId /=) uniqueInvoicesIds

    getInvoiceIdForPendingFees :: (EsqDBReplicaFlow m r, EsqDBFlow m r, MonadFlow m) => [DDF.DriverFee] -> m (Id INV.Invoice)
    getInvoiceIdForPendingFees pendingFees = do
      invoices <- (runInReplica . QINV.findActiveManualInvoiceByFeeId . (.id)) `mapM` pendingFees
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

    inactivateInvoices :: (EsqDBFlow m r, MonadFlow m) => [INV.Invoice] -> m ()
    inactivateInvoices = mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id))

    mergeSortAndRemoveDuplicate :: [[INV.Invoice]] -> [INV.Invoice]
    mergeSortAndRemoveDuplicate invoices = do
      let uniqueInvoices = DL.nubBy (\x y -> x.id == y.id) (concat invoices)
      sortOn (Down . (.createdAt)) uniqueInvoices

    insertInvoiceAgainstDriverFee :: (EsqDBFlow m r, MonadFlow m) => [DDF.DriverFee] -> m (Id INV.Invoice)
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
          updatedAt = now,
          createdAt = now
        }

    buildDriverFeeByInvoice ::
      (EsqDBReplicaFlow m r, MonadFlow m) =>
      [DDF.DriverFee] ->
      Maybe DDF.DriverFeeStatus ->
      Id INV.Invoice ->
      m DriverFeeByInvoice
    buildDriverFeeByInvoice driverFees mStatus invoiceId = do
      invoices <- runInReplica $ QINV.findById invoiceId
      now <- getCurrentTime
      let driverFeeIds = invoices <&> (.driverFeeId)
          invoiceDriverFees = filter (\x -> x.id `elem` driverFeeIds) driverFees
          date = utctDay $ maybe now (.createdAt) (listToMaybe invoices)
          numRides = sum (invoiceDriverFees <&> (.numRides))
          payBy = minimum (invoiceDriverFees <&> (.payBy))
          startTime = minimum (invoiceDriverFees <&> (.startTime))
          endTime = maximum (invoiceDriverFees <&> (.endTime))
          totalEarnings = sum (invoiceDriverFees <&> (.totalEarnings))
          govtCharges = sum (invoiceDriverFees <&> fromIntegral . (.govtCharges))
          fee = sum (invoiceDriverFees <&> (.platformFee.fee))
          cgst = sum (invoiceDriverFees <&> (.platformFee.cgst))
          sgst = sum (invoiceDriverFees <&> (.platformFee.sgst))
          platformFee = PlatformFee {..}
          totalFee = round $ govtCharges + platformFee.fee + platformFee.cgst + platformFee.sgst
          status =
            case mStatus of
              (Just status_) -> status_
              Nothing -> maybe DDF.INACTIVE (.status) (listToMaybe invoiceDriverFees)

      return $ DriverFeeByInvoice {..}

changeAutoPayFeesAndInvoicesForDriverFeesToManual :: MonadFlow m => [Id DDF.DriverFee] -> [Id DDF.DriverFee] -> m ()
changeAutoPayFeesAndInvoicesForDriverFeesToManual alldriverFeeIdsInBatch validDriverFeeIds = do
  let driverFeeIdsToBeShiftedToManual = alldriverFeeIdsInBatch \\ validDriverFeeIds
  QDF.updateToManualFeeByDriverFeeIds driverFeeIdsToBeShiftedToManual
  QINV.updateInvoiceStatusByDriverFeeIds INV.INACTIVE driverFeeIdsToBeShiftedToManual

roundToHalf :: HighPrecMoney -> HighPrecMoney
roundToHalf x = fromInteger (round (x * 2)) / 2
