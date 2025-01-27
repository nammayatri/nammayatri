{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.Revenue
  ( getRevenueCollectionHistory,
    getRevenueAllFeeHistory,
  )
where

import qualified "this" API.Types.Dashboard.RideBooking.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Revenue as Common
import Data.Maybe
import Data.Text hiding (drop, elem, filter, length, map)
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (drop, id, take, unpack)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, fromMaybeM, throwError)
import Kernel.Utils.Time
import SharedLogic.Merchant
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.DriverFee as CHDriverFee
import Storage.Queries.Volunteer (findAllByPlace)
import Tools.Error

getRevenueAllFeeHistory :: ShortId DM.Merchant -> Context.City -> Maybe UTCTime -> Maybe UTCTime -> Flow [Common.AllFees]
getRevenueAllFeeHistory merchantShortId opCity mbFrom mbTo = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  allFees <- CHDriverFee.findAllByStatusSubSelect merchant.id [Common.CLEARED, Common.COLLECTED_CASH, Common.EXEMPTED, Common.PAYMENT_PENDING, Common.PAYMENT_OVERDUE] (Just from) (Just to)
  getAllFeeFromDriverFee `mapM` allFees

getAllFeeFromDriverFee :: MonadFlow m => CHDriverFee.DriverFeeAggregated -> m Common.AllFees
getAllFeeFromDriverFee CHDriverFee.DriverFeeAggregated {..} = do
  case (statusAgg, numRidesAgg, totalAmount, specialZoneAmt) of
    (Just status, Just numRides, Just totalAmount_, Just specialZoneAmount_) -> do
      pure $ Common.AllFees {status, numRides, numDrivers, totalAmount = round totalAmount_, specialZoneAmount = round specialZoneAmount_, openMarketAmount = round (totalAmount_ - specialZoneAmount_)}
    _ -> throwError $ InvalidRequest "Couldn't find driver fee"

getRevenueCollectionHistory :: ShortId DM.Merchant -> Context.City -> Maybe UTCTime -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Flow Common.CollectionList
getRevenueCollectionHistory merchantShortId opCity mbFrom place mbTo volunteerId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let defaultFrom = UTCTime (utctDay now) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  let dayBasis = diffUTCTime to from_ > 24 * 60 * 60
  offlineCollectionFees <- case (place, volunteerId) of
    (Nothing, Nothing) -> CHDriverFee.findAllByDateSubSelect merchant.id [Common.COLLECTED_CASH] (Just from_) (Just to) dayBasis Nothing
    (Nothing, Just cId) -> CHDriverFee.findAllByDateSubSelect merchant.id [Common.COLLECTED_CASH] (Just from_) (Just to) dayBasis (Just [Id cId])
    (Just stn, _) -> do
      volunteers <- findAllByPlace stn
      let relevantIds = case volunteerId of
            Just cId -> [Id cId | cId `elem` ((.id.getId) <$> volunteers)]
            _ -> (.id) <$> volunteers
      CHDriverFee.findAllByDateSubSelect merchant.id [Common.COLLECTED_CASH] (Just from_) (Just to) dayBasis (Just relevantIds)
  onlineCollectionFees <- CHDriverFee.findAllByDateSubSelect merchant.id [Common.CLEARED] (Just from_) (Just to) dayBasis Nothing
  onlineCollection <- getCollectionListElem `mapM` onlineCollectionFees
  offlineCollection <- getCollectionListElem `mapM` offlineCollectionFees
  pure $ Common.CollectionList {onlineCollection, offlineCollection}

getCollectionListElem :: MonadFlow m => CHDriverFee.DriverFeeAggregated -> m Common.CollectionListElem
getCollectionListElem CHDriverFee.DriverFeeAggregated {..} = do
  case (totalAmount, specialZoneAmt, numRidesAgg, date, hour) of
    (Just totalAmount_, Just specialZoneAmount_, Just totalRides, Just date_, Just hour_) ->
      pure $
        Common.CollectionListElem
          { totalAmount = round totalAmount_,
            specialZoneAmount = round specialZoneAmount_,
            openMarketAmount = round (totalAmount_ - specialZoneAmount_),
            date = date_,
            hour = hour_,
            ..
          }
    _ -> throwError $ InvalidRequest "Couldn't find driver fee"
