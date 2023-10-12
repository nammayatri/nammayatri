{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Revenue
  ( getCollectionHistory,
    getAllDriverFeeHistory,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Revenue as Common
import qualified Data.List as DL
import Data.Maybe
import Data.Text hiding (drop, filter, length, map, take)
import Data.Time hiding (getCurrentTime)
import Domain.Types.DriverFee
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common (HighPrecMoney)
import Kernel.Utils.Time
import SharedLogic.Merchant
import Storage.Queries.DriverFee (findAllByStatus, findAllByTimeMerchantAndStatus, findAllByVolunteerIds)
import Storage.Queries.Volunteer (findAllByPlace)

getAllDriverFeeHistory :: ShortId DM.Merchant -> Maybe UTCTime -> Maybe UTCTime -> Flow [Common.AllFees]
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
  pure
    [ Common.AllFees Common.CLEARED (getNumRides clearedFees) (getNumDrivers clearedFees) (getTotalAmount clearedFees),
      Common.AllFees Common.COLLECTED_CASH (getNumRides cashedFees) (getNumDrivers cashedFees) (getTotalAmount cashedFees),
      Common.AllFees Common.PAYMENT_PENDING (getNumRides pendingFees) (getNumDrivers pendingFees) (getTotalAmount pendingFees),
      Common.AllFees Common.PAYMENT_OVERDUE (getNumRides overdueFees) (getNumDrivers overdueFees) (getTotalAmount overdueFees),
      Common.AllFees Common.EXEMPTED (getNumRides exemptedFees) (getNumDrivers exemptedFees) (getTotalAmount exemptedFees)
    ]
  where
    getNumDrivers fee = length $ DL.nub (map driverId fee)
    getFeeWithStatus status = filter (\fee_ -> fee_.status `elem` status)
    getNumRides fee = sum $ map numRides fee
    getTotalAmount fee = sum $ map (\fee_ -> fromIntegral fee_.govtCharges + fee_.platformFee.fee + fee_.platformFee.cgst + fee_.platformFee.sgst) fee

getCollectionHistory :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.CollectionList
getCollectionHistory merchantShortId volunteerId place mbFrom mbTo = do
  now <- getCurrentTime
  merchant <- findMerchantByShortId merchantShortId
  let defaultFrom = UTCTime (fromGregorian 2020 1 1) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  offlineCollections <- case (place, volunteerId) of
    (Nothing, Nothing) -> findAllByStatus merchant.id COLLECTED_CASH from_ to
    (Nothing, Just cId) -> findAllByVolunteerIds merchant.id [cId] from_ to
    (Just stn, _) -> do
      volunteers <- findAllByPlace stn
      let relevantIds = case volunteerId of
            Just cId -> [cId | cId `elem` ((.id.getId) <$> volunteers)]
            _ -> (.id.getId) <$> volunteers
      findAllByVolunteerIds merchant.id relevantIds from_ to
  onlineCollections <- findAllByStatus merchant.id CLEARED from_ to
  let offlineCollectionsRes = getCollectionSummary offlineCollections
  let onlineCollectionsRes = getCollectionSummary onlineCollections
  pure $ Common.CollectionList onlineCollectionsRes offlineCollectionsRes

getCollectionSummary :: [DriverFee] -> Common.CollectionListRes
getCollectionSummary collections = do
  let totalCnt = length collections
      totalFeeCollected = sum $ map calcFee collections
      totalRides = sum $ map (.numRides) collections
      totalDrivers = length $ DL.nub (map driverId collections)
      collectionsTs =
        [ (calculateTotalFee tmp, listToMaybe [x | (_, Just x) <- tmp])
          | tmp <-
              DL.groupBy (\(_, a) (_, b) -> a == b) $
                sortBy (comparing snd) [(calcFee fee, fee.collectedAt) | fee <- collections]
        ]
      numRidesTs =
        [ (calculateTotalRides tmp, listToMaybe [x | (_, Just x) <- tmp])
          | tmp <-
              DL.groupBy (\(_, a) (_, b) -> a == b) $
                sortBy (comparing snd) [(fee.numRides, fee.collectedAt) | fee <- collections]
        ]
      driversPmtTs = [(length tmp, listToMaybe tmp) | tmp <- DL.group $ DL.sort $ catMaybes [fee.collectedAt | fee <- collections]]
  Common.CollectionListRes totalCnt totalFeeCollected totalRides totalDrivers collectionsTs numRidesTs driversPmtTs
  where
    calcFee fee = fromIntegral fee.govtCharges + fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst

calculateTotalFee :: [(HighPrecMoney, Maybe UTCTime)] -> HighPrecMoney
calculateTotalFee tmp = sum [fee | (fee, _) <- tmp]

calculateTotalRides :: [(Int, Maybe UTCTime)] -> Int
calculateTotalRides tmp = sum [numberOfRides | (numberOfRides, _) <- tmp]
