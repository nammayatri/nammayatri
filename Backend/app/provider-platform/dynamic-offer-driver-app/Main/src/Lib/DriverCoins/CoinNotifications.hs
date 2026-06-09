{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverCoins.CoinNotifications
  ( sendCoinsNotification,
    sendCoinsNotificationV2,
    sendCoinsNotificationV3,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Types as L
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Translations as MTQuery
import Tools.Error
import qualified Tools.Notifications as Notify

type EventFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, MonadReader r m, ClickhouseFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r)

sendCoinsNotificationV3 :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotificationV3 merchantOpCityId driverId coinsValue (DCT.MetroRideCompleted metroRideType _) =
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
        entityData = Notify.CoinsNotificationData {coins = coinsValue, event = DCT.MetroRideCompleted metroRideType Nothing}
     in MTQuery.findByErrorAndLanguage (T.pack (show metroRideType)) language >>= processMessage driver entityData
  where
    processMessage driver entityData mbCoinsMessage =
      case mbCoinsMessage of
        Just coinsMessage ->
          case T.splitOn " | " coinsMessage.message of
            [title, description] ->
              Notify.sendCoinsNotificationV3 merchantOpCityId title (replaceCoinsValue description) driver (driver.deviceToken) entityData metroRideType
            _ -> logDebug "Invalid message format."
        Nothing -> logDebug "Could not find Translations."
    replaceCoinsValue = T.replace "{#pointsValue#}" (T.pack $ show coinsValue)
sendCoinsNotificationV3 _merchantOpCityId _driverId _coinsValue _eventFunction = pure ()

sendCoinsNotificationV2 :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> HighPrecMoney -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotificationV2 merchantOpCityId driverId amount coinsValue (DCT.BulkUploadFunctionV2 messageKey) = do
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
     in MTQuery.findByErrorAndLanguage (T.pack (show messageKey)) language >>= processMessage driver amount
  where
    processMessage driver amount' mbCoinsMessage =
      case mbCoinsMessage of
        Just coinsMessage ->
          case T.splitOn " | " coinsMessage.message of
            [title, description] -> do
              let descriptionReplaced = replaceAmountValue amount' $ replaceCoinsValue description
              Notify.sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing FCM.COINS_SUCCESS title descriptionReplaced driver (driver.deviceToken)
            _ -> logDebug "Invalid message format."
        Nothing -> logDebug "Could not find Translations."
    replaceCoinsValue = T.replace "{#coinsValue#}" (T.pack $ show coinsValue)
    replaceAmountValue amount' = T.replace "{#amountValue#}" (T.pack $ show amount')
sendCoinsNotificationV2 _merchantOpCityId _driverId _amount _coinsValue _eventFunction = pure ()

sendCoinsNotification :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotification merchantOpCityId driverId coinsValue eventFunction =
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
        queryType = if coinsValue > 0 then DCT.CoinAdded else DCT.CoinSubtracted
        entityData = Notify.CoinsNotificationData {coins = coinsValue, event = eventFunction}
     in MTQuery.findByErrorAndLanguage (T.pack (show queryType)) language >>= processMessage driver entityData
  where
    processMessage driver entityData mbCoinsMessage =
      case mbCoinsMessage of
        Just coinsMessage ->
          case T.splitOn " | " coinsMessage.message of
            [title, description] ->
              Notify.sendCoinsNotification merchantOpCityId title (replaceCoinsValue description) driver (driver.deviceToken) entityData
            _ -> logDebug "Invalid message format."
        Nothing -> logDebug "Could not find Translations."
    replaceCoinsValue = T.replace "{#coinsValue#}" (T.pack $ show coinsValue)
