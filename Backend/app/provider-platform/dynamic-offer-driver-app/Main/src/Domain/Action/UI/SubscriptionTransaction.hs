{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SubscriptionTransaction
  ( getSubscriptionTransactions,
  )
where

import API.Types.UI.SubscriptionTransaction
import qualified Data.Map as M
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Dashboard.Common as DCommon
import Domain.Types.DriverWallet
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.Finance.Prepaid
import qualified Storage.Queries.BookingExtra as QBookingE
import qualified Storage.Queries.Person as QP

getSubscriptionTransactions ::
  (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe Int ->
  Maybe EntryStatus ->
  Maybe UTCTime ->
  m SubscriptionTransactionResponse
getSubscriptionTransactions (mbDriverId, _, _) mbFrom mbLimit mbMaxAmount mbMinAmount mbOffset mbStatus mbTo = do
  driverId' <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  driver <- QP.findById driverId' >>= fromMaybeM (PersonNotFound driverId'.getId)
  now <- getCurrentTime
  let fromDate = fromMaybe (UTCTime (utctDay now) 0) mbFrom
      toDate = fromMaybe now mbTo
      limit = min maxLimit . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset
      counterpartyType = if DCommon.checkFleetOwnerRole driver.role then counterpartyFleetOwner else counterpartyDriver
  mbAccount <- getPrepaidAccountByOwner counterpartyType driverId'.getId
  case mbAccount of
    Nothing -> pure emptyResponse
    Just account -> do
      let referenceTypes = [prepaidRideDebitReferenceType, subscriptionCreditReferenceType]
      entries <-
        findByAccountWithFilters
          account.id
          (Just fromDate)
          (Just toDate)
          mbMinAmount
          mbMaxAmount
          mbStatus
          (Just referenceTypes)
      let downSorted = sortOn (Down . (.timestamp)) entries
          upSorted = sortOn (.timestamp) entries
          rideEntries = filter (\e -> e.referenceType == prepaidRideDebitReferenceType) downSorted
          planEntries = filter (\e -> e.referenceType == subscriptionCreditReferenceType) downSorted
          pagedRides = take limit . drop offset $ rideEntries
          pagedPlans = take limit . drop offset $ planEntries
          paged = sortOn (Down . (.timestamp)) (pagedRides <> pagedPlans)
          rideEarning = sum $ map (.amount) rideEntries
          planPurchased = sum $ map (.amount) planEntries
          startingBalance = computeStartingBalance account.id upSorted
          finalBalance = computeFinalBalance account.id upSorted
      -- Enrich ride entries with booking locations
      let bookingIds = mapMaybe (\e -> if e.referenceType == prepaidRideDebitReferenceType then Just (Id e.referenceId) else Nothing) paged
      bookings <- if null bookingIds then pure [] else QBookingE.findByIds bookingIds
      let bookingMap = M.fromList $ map (\b -> (b.id, (b.fromLocation, b.toLocation))) bookings
      entities <- forM paged $ \entry -> do
        let transactionType = if entry.referenceType == prepaidRideDebitReferenceType then RIDE else PLAN_PURCHASE
            (fromLocation, toLocation) =
              if entry.referenceType == prepaidRideDebitReferenceType
                then case M.lookup (Id entry.referenceId) bookingMap of
                  Just (fromLoc, toLoc) -> (Just fromLoc, toLoc)
                  Nothing -> (Nothing, Nothing)
                else (Nothing, Nothing)
        pure
          SubscriptionTransactionEntity
            { driverId = driverId'.getId,
              entityId = Just entry.referenceId,
              transactionType,
              amount = entry.amount,
              status = entry.status,
              fromLocation,
              toLocation,
              createdAt = entry.timestamp,
              updatedAt = entry.updatedAt
            }
      pure
        SubscriptionTransactionResponse
          { startingBalance,
            finalBalance,
            rideEarning,
            planPurchased,
            entities
          }
  where
    maxLimit = 10
    emptyResponse =
      SubscriptionTransactionResponse
        { startingBalance = 0,
          finalBalance = 0,
          rideEarning = 0,
          planPurchased = 0,
          entities = []
        }
    computeStartingBalance accountId entries =
      case entries of
        [] -> 0
        (firstEntry : _) ->
          if firstEntry.fromAccountId == accountId
            then fromMaybe 0 firstEntry.fromStartingBalance
            else fromMaybe 0 firstEntry.toStartingBalance
    computeFinalBalance accountId entries =
      case reverse entries of
        [] -> 0
        (lastEntry : _) ->
          if lastEntry.fromAccountId == accountId
            then fromMaybe 0 lastEntry.fromEndingBalance
            else fromMaybe 0 lastEntry.toEndingBalance
