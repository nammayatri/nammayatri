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

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Revenue as Common
import Data.Maybe
import Data.Text hiding (drop, elem, filter, length, map)
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (drop, id, take, unpack)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Clickhouse.Operators
import qualified Kernel.Storage.Clickhouse.Queries as CH
import qualified Kernel.Storage.Clickhouse.Types as CH
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, fromMaybeM, logError, throwError)
import Kernel.Utils.Time
import SharedLogic.Merchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import Storage.Queries.Volunteer (findAllByPlace)
import Tools.Error

getAllDriverFeeHistory :: ShortId DM.Merchant -> Context.City -> Maybe UTCTime -> Maybe UTCTime -> Flow [Common.AllFees]
getAllDriverFeeHistory merchantShortId opCity mbFrom mbTo = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
      cols = ["status", "SUM(num_rides) AS numRides", "COUNT(DISTINCT driver_id) AS numDrivers", "SUM(platform_fee + cgst + sgst + govt_charges) AS totalAmount"]
      groupBy_ = Just "status"
  pendingAndOverdueFees <- CH.findAll ATLAS_DRIVER_OFFER_BPP (Proxy @Common.DriverFee) cols groupBy_ (("merchant_id" =.= unpack merchant.id.getId) &.& ("status" `__in` ["\'PAYMENT_PENDING\', \'PAYMENT_OVERDUE\'"])) Nothing Nothing Nothing
  otherFees <- CH.findAll ATLAS_DRIVER_OFFER_BPP (Proxy @Common.DriverFee) cols groupBy_ (("merchant_id" =.= unpack merchant.id.getId) &.& ("status" `__in` ["\'CLEARED\'", "\'COLLECTED_CASH\'", "\'EXEMPTED\'"]) &.& ("collected_at" >>==. fetchDatetime (show from)) &.& ("collected_at" <<==. fetchDatetime (show to))) Nothing Nothing Nothing
  dueFees <- getCkhData pendingAndOverdueFees
  paidFees <- getCkhData otherFees
  getAllFeeFromDriverFee `mapM` (dueFees <> paidFees)

getAllFeeFromDriverFee :: MonadFlow m => Common.DriverFee -> m Common.AllFees
getAllFeeFromDriverFee Common.DriverFee {..} = do
  case (status, numRides, numDrivers, totalAmount) of
    (Just status_, Just numRides_, Just numDrivers_, Just totalAmount_) -> do
      status' <- readMaybe status_ & fromMaybeM (InvalidRequest "Couldn't find fee's status")
      numRides' <- readMaybe numRides_ & fromMaybeM (InvalidRequest "Couldn't find fee's ride count")
      numDrivers' <- readMaybe numDrivers_ & fromMaybeM (InvalidRequest "Couldn't find fee's driver count")
      pure $ Common.AllFees status' numRides' numDrivers' (round totalAmount_)
    _ -> throwError $ InvalidRequest "Couldn't find driver fee"

getCollectionHistory :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.CollectionList
getCollectionHistory merchantShortId opCity volunteerId place mbFrom mbTo = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let defaultFrom = UTCTime (utctDay now) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  let dayBasis = diffUTCTime to from_ > 24 * 60 * 60
      colsWithoutHr = ["SUM(platform_fee + cgst + sgst + govt_charges) AS totalAmount", "SUM(num_rides) AS numRides", "COUNT(DISTINCT driver_id) AS numDrivers", "toDate(collected_at) AS date"]
      groupBy_ = Just "date, hour"
      cols = if dayBasis then colsWithoutHr <> ["0 AS hour"] else colsWithoutHr <> ["toHour(collected_at) AS hour"]
  mbOfflineCollections <- case (place, volunteerId) of
    (Nothing, Nothing) -> CH.findAll ATLAS_DRIVER_OFFER_BPP (Proxy @Common.DriverFee) cols groupBy_ (("merchant_id" =.= unpack merchant.id.getId) &.& ("status" =.= "COLLECTED_CASH") &.& ("collected_at" >>==. fetchDatetime (show from_)) &.& ("collected_at" <<==. fetchDatetime (show to))) Nothing Nothing (Just $ CH.Asc "date, hour")
    (Nothing, Just cId) -> CH.findAll ATLAS_DRIVER_OFFER_BPP (Proxy @Common.DriverFee) cols groupBy_ (("merchant_id" =.= unpack merchant.id.getId) &.& ("status" =.= "COLLECTED_CASH") &.& ("collected_at" >>==. fetchDatetime (show from_)) &.& ("collected_at" <<==. fetchDatetime (show to)) &.& ("collected_by" =.= unpack cId)) Nothing Nothing (Just $ CH.Asc "date, hour")
    (Just stn, _) -> do
      volunteers <- findAllByPlace stn
      let relevantIds = case volunteerId of
            Just cId -> [cId | cId `elem` ((.id.getId) <$> volunteers)]
            _ -> (.id.getId) <$> volunteers
      CH.findAll ATLAS_DRIVER_OFFER_BPP (Proxy @Common.DriverFee) cols groupBy_ (("merchant_id" =.= unpack merchant.id.getId) &.& ("status" =.= "COLLECTED_CASH") &.& ("collected_at" >>==. fetchDatetime (show from_)) &.& ("collected_at" <<==. fetchDatetime (show to)) &.& ("collected_by" `__in` (map (\str -> "'" ++ str ++ "'") unpack <$> relevantIds))) Nothing Nothing (Just $ CH.Asc "date, hour")
  mbOnlineCollections <- CH.findAll ATLAS_DRIVER_OFFER_BPP (Proxy @Common.DriverFee) cols groupBy_ (("merchant_id" =.= unpack merchant.id.getId) &.& ("status" =.= "CLEARED") &.& ("collected_at" >>==. fetchDatetime (show from_)) &.& ("collected_at" <<==. fetchDatetime (show to))) Nothing Nothing (Just $ CH.Asc "date, hour")
  offlineCollectionFees <- getCkhData mbOfflineCollections
  onlineCollectionFees <- getCkhData mbOnlineCollections
  onlineCollection <- getCollectionListElem `mapM` onlineCollectionFees
  offlineCollection <- getCollectionListElem `mapM` offlineCollectionFees
  pure $ Common.CollectionList onlineCollection offlineCollection

getCollectionListElem :: MonadFlow m => Common.DriverFee -> m Common.CollectionListElem
getCollectionListElem Common.DriverFee {..} = do
  case (totalAmount, numRides, numDrivers, date, hour) of
    (Just totalAmount_, Just numRides_, Just numDrivers_, Just date_, Just hour_) -> do
      numRides' <- readMaybe numRides_ & fromMaybeM (InvalidRequest "Couldn't find fee's ride count")
      numDrivers' <- readMaybe numDrivers_ & fromMaybeM (InvalidRequest "Couldn't find fee's driver count")
      date' <- readMaybe date_ & fromMaybeM (InvalidRequest "Couldn't find fee's date")
      pure $ Common.CollectionListElem (round totalAmount_) numRides' numDrivers' date' hour_
    _ -> throwError $ InvalidRequest "Couldn't find driver fee"

fetchDatetime :: Text -> String
fetchDatetime dateTime = unpack $ take 19 dateTime

getCkhData :: (MonadFlow m) => Either String [a] -> m [a]
getCkhData tbl = case tbl of
  Left err -> do
    logError $ "Clickhouse error: " <> show err
    pure []
  Right y -> pure y
