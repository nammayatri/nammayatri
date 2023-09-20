{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Revenue
  ( getCashCollectionHistory,
    getAllDriverFeeHistory,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Revenue as Common
import Data.List (nub)
import Data.Maybe
import Data.Text hiding (drop, filter, length, map, take)
import Data.Time hiding (getCurrentTime)
import Domain.Types.DriverFee
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Time
import SharedLogic.Merchant
import Storage.Queries.DriverFee (findAllByCollectorId, findAllByTimeMerchantAndStatus)

getAllDriverFeeHistory :: ShortId DM.Merchant -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.AllDriverFeeRes
getAllDriverFeeHistory merchantShortId mbFrom mbTo = do
  now <- getCurrentTime
  merchant <- findMerchantByShortId merchantShortId
  let defaultFrom = UTCTime (fromGregorian 2020 1 1) 0 -- can be in common
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  allDriverFee <- findAllByTimeMerchantAndStatus merchant.id from to [PAYMENT_PENDING, PAYMENT_OVERDUE, EXEMPTED, CLEARED, COLLECTED_CASH]
  let clearedFees = getFeeWithStatus [CLEARED] allDriverFee
      pendingFees = getFeeWithStatus [PAYMENT_PENDING] allDriverFee
      overdueFees = getFeeWithStatus [PAYMENT_OVERDUE] allDriverFee
      exemptedFees = getFeeWithStatus [EXEMPTED] allDriverFee
      cashedFees = getFeeWithStatus [COLLECTED_CASH] allDriverFee
      yetToPayFees = getFeeWithStatus [PAYMENT_PENDING, PAYMENT_OVERDUE] allDriverFee
      driversPaidOnline = getNumDrivers clearedFees
      driversPaidOffline = getNumDrivers cashedFees
      driversYetToPay = getNumDrivers yetToPayFees
  pure $
    Common.AllDriverFeeRes
      driversPaidOnline
      driversPaidOffline
      driversYetToPay
      [ Common.AllFees Common.CLEARED (getNumRides clearedFees) (getTotalAmount clearedFees),
        Common.AllFees Common.COLLECTED_CASH (getNumRides cashedFees) (getTotalAmount cashedFees),
        Common.AllFees Common.PAYMENT_PENDING (getNumRides pendingFees) (getTotalAmount pendingFees),
        Common.AllFees Common.PAYMENT_OVERDUE (getNumRides overdueFees) (getTotalAmount overdueFees),
        Common.AllFees Common.EXEMPTED (getNumRides exemptedFees) (getTotalAmount exemptedFees)
      ]
  where
    getNumDrivers fee = length $ nub (map driverId fee)
    getFeeWithStatus status = filter (\fee_ -> fee_.status `elem` status)
    getNumRides fee = sum $ map numRides fee
    getTotalAmount fee = sum $ map (\fee_ -> fromIntegral fee_.govtCharges + fee_.platformFee.fee + fee_.platformFee.cgst + fee_.platformFee.sgst) fee

getCashCollectionHistory :: ShortId DM.Merchant -> Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> Flow Common.CashCollectionListRes
getCashCollectionHistory merchantShortId collectorId mbFrom mbTo mbLimit mbOffset = do
  now <- getCurrentTime
  merchant <- findMerchantByShortId merchantShortId
  let limit_ = fromMaybe defaultLimit mbLimit
      offset_ = fromMaybe 0 mbOffset
      defaultFrom = UTCTime (fromGregorian 2020 1 1) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  collections <- findAllByCollectorId merchant.id collectorId from_ to limit_ offset_
  let totalCnt = length collections
      totalFee = sum $ map (\fee -> fromIntegral fee.govtCharges + fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst) collections
  let limitresp = min maxLimit . fromMaybe defaultLimit $ mbLimit
  let collectionList = map buildCollectionList (limitOffset limitresp offset_ collections)
  let count_ = length collectionList
  pure $ Common.CashCollectionListRes (Common.Summary totalCnt count_) totalFee collectionList
  where
    maxLimit = 20
    defaultLimit = 10

    buildCollectionList DriverFee {..} = do
      Common.DriverFeeAPIEntity
        { id = cast id,
          merchantId = merchantId.getId,
          driverId = cast driverId,
          fee = fromIntegral govtCharges + platformFee.fee + platformFee.cgst + platformFee.sgst,
          ..
        }

    limitOffset :: Int -> Int -> [a] -> [a] -- can be in common
    limitOffset limitVal offsetVal xs = take limitVal (drop offsetVal xs)
