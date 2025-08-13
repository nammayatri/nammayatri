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
import Data.Time hiding (getCurrentTime)
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.SubscriptionTransaction
import EulerHS.Prelude hiding (id)
import Kernel.External.Payment.Juspay.Types.Common
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.SubscriptionTransactionExtra as QSubscriptionTransaction
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
  Maybe TransactionStatus ->
  Maybe UTCTime ->
  m [SubscriptionTransactionEntity]
getSubscriptionTransactions (mbDriverId, _, _) mbFrom mbLimit mbMaxAmount mbMinAmount mbOffset mbStatus mbTo = do
  driverId' <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  now <- getCurrentTime
  let fromDate = fromMaybe (UTCTime (utctDay now) 0) mbFrom
      toDate = fromMaybe now mbTo
      limit = min maxLimit . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset
  transactions <- QSubscriptionTransaction.findAllByDriverDateAmountAndStatus driverId' fromDate toDate mbStatus mbMinAmount mbMaxAmount limit offset
  let rideIds = mapMaybe (\st -> if st.transactionType == RIDE then st.entityId else Nothing) transactions
  (rideMap, bookingMap) <- Utils.fetchRideLocationData (map Id rideIds)
  forM transactions $ \SubscriptionTransaction {..} -> do
    (fromLocation, toLocation) <- case transactionType of
      RIDE -> Utils.extractLocationFromMaps (Id <$> entityId) rideMap bookingMap
      _ -> return (Nothing, Nothing)
    return $
      SubscriptionTransactionEntity
        { driverId = driverId'.getId,
          ..
        }
  where
    maxLimit = 10
