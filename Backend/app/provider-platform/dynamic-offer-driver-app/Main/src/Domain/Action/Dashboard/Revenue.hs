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
import Data.Text hiding (drop, elem, filter, length, map, take)
import Data.Time hiding (getCurrentTime)
import Domain.Types.DriverFee
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as KS
import Kernel.Utils.Common (HighPrecMoney, fromMaybeM)
import Kernel.Utils.SlidingWindowCounters
import Kernel.Utils.Time
import SharedLogic.Merchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import Storage.Queries.DriverFee (findAllByStatus, findAllByVolunteerIds, findAllCollectionInRange, findAllOverdueInRange, findAllPendingInRange)
import Storage.Queries.Volunteer (findAllByPlace)
import Tools.Error

getAllDriverFeeHistory :: ShortId DM.Merchant -> Context.City -> Maybe UTCTime -> Maybe UTCTime -> Flow [Common.AllFees]
getAllDriverFeeHistory merchantShortId opCity mbFrom mbTo = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let defaultFrom = UTCTime (fromGregorian 2020 1 1) 0 -- can be in common
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo

  pendingFees <- B.runInReplica $ findAllPendingInRange merchant.id from to
  overdueFees <- B.runInReplica $ findAllOverdueInRange merchant.id from to
  collectionFees <- B.runInReplica $ findAllCollectionInRange merchant.id from to

  let clearedFees = getFeeWithStatus [CLEARED] collectionFees
      exemptedFees = getFeeWithStatus [EXEMPTED] collectionFees
      cashedFees = getFeeWithStatus [COLLECTED_CASH] collectionFees
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

getCollectionHistory :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.CollectionList
getCollectionHistory merchantShortId _ volunteerId place mbFrom mbTo = do
  now <- getCurrentTime
  merchant <- findMerchantByShortId merchantShortId
  let defaultFrom = UTCTime (fromGregorian 2020 1 1) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  let rangeBasis = if diffUTCTime to from_ > 24 * 60 * 60 then KS.Days else KS.Hours
  offlineCollections <- case (place, volunteerId) of
    (Nothing, Nothing) -> B.runInReplica $ findAllByStatus merchant.id COLLECTED_CASH from_ to
    (Nothing, Just cId) -> B.runInReplica $ findAllByVolunteerIds merchant.id [cId] from_ to
    (Just stn, _) -> do
      volunteers <- findAllByPlace stn
      let relevantIds = case volunteerId of
            Just cId -> [cId | cId `elem` ((.id.getId) <$> volunteers)]
            _ -> (.id.getId) <$> volunteers
      B.runInReplica $ findAllByVolunteerIds merchant.id relevantIds from_ to
  onlineCollections <- B.runInReplica $ findAllByStatus merchant.id CLEARED from_ to
  let offlineCollectionsRes = getCollectionSummary offlineCollections rangeBasis
  let onlineCollectionsRes = getCollectionSummary onlineCollections rangeBasis
  pure $ Common.CollectionList onlineCollectionsRes offlineCollectionsRes

getCollectionSummary :: [DriverFee] -> KS.PeriodType -> Common.CollectionListRes
getCollectionSummary collections granuality = do
  let totalCnt = length collections
      totalFeeCollected = sum $ map calcFee collections
      totalRides = sum $ map (.numRides) collections
      totalDrivers = length $ DL.nub (map driverId collections)
      collectionsTs =
        [ (calculateTotalFee tmp, listToMaybe [x | (_, Just x) <- tmp])
          | tmp <-
              DL.groupBy (\(_, a) (_, b) -> makeGranular a == makeGranular b) $
                sortBy (comparing snd) [(calcFee fee, fee.collectedAt) | fee <- collections]
        ]
      numRidesTs =
        [ (calculateTotalRides tmp, listToMaybe [x | (_, Just x) <- tmp])
          | tmp <-
              DL.groupBy (\(_, a) (_, b) -> makeGranular a == makeGranular b) $
                sortBy (comparing snd) [(fee.numRides, fee.collectedAt) | fee <- collections]
        ]
      driversPmtTs = [(length tmp, listToMaybe (catMaybes tmp)) | tmp <- DL.groupBy (\a b -> makeGranular a == makeGranular b) $ DL.sort [fee.collectedAt | fee <- collections]]
  Common.CollectionListRes totalCnt totalFeeCollected totalRides totalDrivers collectionsTs numRidesTs driversPmtTs
  where
    makeGranular (Just timeVal) = makeSlidingWindowKey granuality "" timeVal
    makeGranular Nothing = ""
    calcFee fee = fromIntegral fee.govtCharges + fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst

calculateTotalFee :: [(HighPrecMoney, Maybe UTCTime)] -> HighPrecMoney
calculateTotalFee tmp = sum [fee | (fee, _) <- tmp]

calculateTotalRides :: [(Int, Maybe UTCTime)] -> Int
calculateTotalRides tmp = sum [numberOfRides | (numberOfRides, _) <- tmp]
