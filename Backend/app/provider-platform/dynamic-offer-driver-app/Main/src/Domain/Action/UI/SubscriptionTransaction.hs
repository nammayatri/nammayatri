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
import Domain.Types.DriverWallet
import Data.List (lookup)
import Data.Time hiding (getCurrentTime)
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import SharedLogic.Finance.Prepaid
import qualified Tools.Utils as Utils

getSubscriptionTransactions ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
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
  m [SubscriptionTransactionEntity]
getSubscriptionTransactions (mbDriverId, _, _) mbFrom mbLimit mbMaxAmount mbMinAmount mbOffset mbStatus mbTo = do
  driverId' <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  let fromDate = fromMaybe (UTCTime (utctDay now) 0) mbFrom
      toDate = fromMaybe now mbTo
      limit = min maxLimit . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbAccount <- getPrepaidAccountByOwner ownerTypeDriver driverId'.getId
  case mbAccount of
    Nothing -> pure []
    Just account -> do
      let referenceTypes = ["RIDE", "PLAN_PURCHASE"]
      allEntries <- findByOwnerWithFilters ownerTypeDriver driverId'.getId Nothing Nothing Nothing Nothing Nothing (Just referenceTypes)
      let relevantEntries = allEntries
      let balanceByEntryId = buildRunningBalances account.id (sortOn (.timestamp) relevantEntries)
      filteredEntries <-
        findByOwnerWithFilters
          ownerTypeDriver
          driverId'.getId
          (Just fromDate)
          (Just toDate)
          mbMinAmount
          mbMaxAmount
          mbStatus
          (Just referenceTypes)
      let sortedFilteredEntries = sortOn (.timestamp) filteredEntries
      let pagedEntries = take limit . drop offset $ sortOn (Down . (.timestamp)) sortedFilteredEntries
      let rideIds = mapMaybe (\e -> if e.referenceType == "RIDE" then Just e.referenceId else Nothing) pagedEntries
      (rideMap, bookingMap) <- Utils.fetchRideLocationData (map Id rideIds)
      forM pagedEntries $ \entry -> do
        let transactionType = if entry.referenceType == "RIDE" then RIDE else PLAN_PURCHASE
        (fromLocation, toLocation) <- case transactionType of
          RIDE -> Utils.extractLocationFromMaps (Just (Id entry.referenceId)) rideMap bookingMap
          _ -> return (Nothing, Nothing)
        let runningBalance = fromMaybe 0 (lookup entry.id balanceByEntryId)
        return
          SubscriptionTransactionEntity
            { driverId = driverId'.getId,
              entityId = Just entry.referenceId,
              transactionType,
              amount = entry.amount,
              status = entry.status,
              runningBalance,
              fromLocation,
              toLocation,
              createdAt = entry.timestamp,
              updatedAt = entry.updatedAt
            }
  where
    maxLimit = 10
    buildRunningBalances accountId entries =
      let step (accBal, accMap) entry =
            let delta =
                  if entry.fromAccountId == accountId
                    then (-1) * entry.amount
                    else
                      if entry.toAccountId == accountId
                        then entry.amount
                        else 0
                newBal = accBal + delta
             in (newBal, accMap <> [(entry.id, newBal)])
       in snd $ foldl step (0, []) entries
