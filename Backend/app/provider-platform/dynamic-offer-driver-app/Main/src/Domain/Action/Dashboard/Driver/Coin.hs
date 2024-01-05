{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver.Coin where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Coin as Common
import Data.Time (UTCTime (UTCTime, utctDay))
import Domain.Types.Coins.CoinHistory (CoinStatus (..))
import qualified Domain.Types.Coins.CoinHistory as DTCC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.Types as DCT
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import qualified Storage.Queries.Coins.CoinHistory as CHistory
import Tools.Error

bulkUploadCoinsHandler :: ShortId DM.Merchant -> Context.City -> Common.BulkUploadCoinsReq -> Flow APISuccess
bulkUploadCoinsHandler merchantShortId opCity Common.BulkUploadCoinsReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchant.id.getId
  mapM_ (\Common.DriverIdListWithCoins {..} -> bulkUpdateByDriverId merchant.id merchantOpCityId (Id driverId :: Id SP.Person) DCT.BulkUploadFunction coins bulkUploadTitle expirationTime transporterConfig) driverIdListWithCoins
  pure Success

bulkUpdateByDriverId ::
  ( MonadFlow m,
    MonadReader r m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasField
      "minTripDistanceForReferralCfg"
      r
      (Maybe HighPrecMeters)
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id SP.Person ->
  DCT.DriverCoinsFunctionType ->
  Int ->
  Common.Translations ->
  Maybe Int ->
  TransporterConfig ->
  m ()
bulkUpdateByDriverId merchantId merchantOpCityId driverId eventFunction coinsValue bulkUploadTitle mbexpirationTime transporterConfig = do
  Coins.updateDriverCoins driverId coinsValue transporterConfig.timeDiffFromUtc
  now <- getCurrentTime
  uuid <- generateGUIDText
  let expiryTime = fmap (\expirationTime -> UTCTime (utctDay $ addUTCTime (fromIntegral expirationTime) now) 0) mbexpirationTime
      status_ = if coinsValue > 0 then Remaining else Used
  let driverCoinEvent =
        DTCC.CoinHistory
          { id = Id uuid,
            driverId = driverId.getId,
            merchantId = merchantId.getId,
            merchantOptCityId = merchantOpCityId.getId,
            eventFunction = eventFunction,
            coins = coinsValue,
            status = status_,
            createdAt = now,
            updatedAt = now,
            expirationAt = expiryTime,
            coinsUsed = 0,
            bulkUploadTitle = Just bulkUploadTitle
          }
  CHistory.updateCoinEvent driverCoinEvent
  pure ()
