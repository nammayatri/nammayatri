{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverWallet
  ( makeWalletRunningBalanceLockKey,
    withDriverWalletAndFYEarningsStore,
    emptyDriverWalletCollection,
    DriverWalletInfo (..),
    DriverWalletCollection (..),
  )
where

import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.DriverWallet as DDW
import qualified Domain.Types.FinancialYearEarnings as DFYE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverWallet as QDW
import qualified Storage.Queries.FinancialYearEarnings as QFYE

data DriverWalletInfo = DriverWalletInfo
  { id :: Id DDW.DriverWallet,
    driverId :: Id DP.Driver,
    merchantId :: Maybe (Id DM.Merchant),
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    financialYearCollectionAmount :: Maybe HighPrecMoney,
    financialYearTdsBaseAmount :: Maybe HighPrecMoney,
    financialYearTdsDeduction :: Maybe HighPrecMoney,
    financialYearStart :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

data DriverWalletCollection = DriverWalletCollection
  { collectionAmount :: Maybe HighPrecMoney,
    gstDeduction :: Maybe HighPrecMoney,
    tdsBaseAmount :: Maybe HighPrecMoney,
    tdsDeduction :: Maybe HighPrecMoney
  }

emptyDriverWalletCollection :: DriverWalletCollection
emptyDriverWalletCollection =
  DriverWalletCollection
    { collectionAmount = Nothing,
      gstDeduction = Nothing,
      tdsBaseAmount = Nothing,
      tdsDeduction = Nothing
    }

withDriverWalletAndFYEarningsStore ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  DTC.TransporterConfig ->
  DriverWalletCollection ->
  (Maybe DDW.DriverWallet -> DriverWalletCollection -> DriverWalletInfo -> DDW.DriverWallet) ->
  m ()
withDriverWalletAndFYEarningsStore driverId transporterConfig collection@DriverWalletCollection {..} mkDriverWallet = do
  newId <- generateGUID
  now <- getCurrentTime
  lastTransaction <- QDW.findLatestByDriverId driverId
  let timeDiffFromUtc = transporterConfig.timeDiffFromUtc
      currentFinancialYearStart = getFinancialYearStart timeDiffFromUtc now
      lastFinancialYearStart = case lastTransaction of
        Just lastTransaction' -> fromMaybe (getFinancialYearStart timeDiffFromUtc lastTransaction'.createdAt) lastTransaction'.financialYearStart
        Nothing -> currentFinancialYearStart

      lastFinancialYearCollectionAmount = fromMaybe 0 (lastTransaction >>= (.financialYearCollectionAmount))
      lastFinancialYearTdsBaseAmount = fromMaybe 0 (lastTransaction >>= (.financialYearTdsBaseAmount))
      lastFinancialYearTdsDeduction = fromMaybe 0 (lastTransaction >>= (.financialYearTdsDeduction))
      (financialYearCollectionAmount, financialYearTdsBaseAmount, financialYearTdsDeduction) =
        if currentFinancialYearStart == lastFinancialYearStart
          then (fromMaybe 0 collectionAmount + lastFinancialYearCollectionAmount, fromMaybe 0 tdsBaseAmount + lastFinancialYearTdsBaseAmount, fromMaybe 0 tdsDeduction + lastFinancialYearTdsDeduction)
          else (fromMaybe 0 collectionAmount, fromMaybe 0 tdsBaseAmount, fromMaybe 0 tdsDeduction)

      driverWalletInfo =
        DriverWalletInfo
          { id = newId,
            driverId = cast @DP.Person @DP.Driver driverId,
            merchantId = Just transporterConfig.merchantId,
            merchantOperatingCityId = transporterConfig.merchantOperatingCityId,
            financialYearCollectionAmount = Just financialYearCollectionAmount,
            financialYearTdsBaseAmount = Just financialYearTdsBaseAmount,
            financialYearTdsDeduction = Just financialYearTdsDeduction,
            financialYearStart = Just currentFinancialYearStart,
            createdAt = now,
            updatedAt = now
          }
  let driverWallet = mkDriverWallet lastTransaction collection driverWalletInfo

  unless (currentFinancialYearStart == lastFinancialYearStart) $ do
    logDebug $ "New financial year started: " <> show currentFinancialYearStart <> "; last financial year: " <> show lastFinancialYearStart <> "; driverId: " <> driverId.getId
    fyEarningsId <- generateGUID
    let fyEarnings =
          DFYE.FinancialYearEarnings
            { id = fyEarningsId,
              merchantId = transporterConfig.merchantId,
              merchantOperatingCityId = transporterConfig.merchantOperatingCityId,
              personId = driverId,
              financialYearCollectionAmount = lastFinancialYearCollectionAmount,
              financialYearTdsBaseAmount = lastFinancialYearTdsBaseAmount,
              financialYearTdsDeduction = lastFinancialYearTdsDeduction,
              financialYearStart = lastFinancialYearStart,
              createdAt = now
            }
    QFYE.create fyEarnings
  let lastRunningBalance = maybe 0 (.runningBalance) lastTransaction
  when (lastRunningBalance /= driverWallet.runningBalance) $
    QDI.updateWalletBalance (Just driverWallet.runningBalance) driverId
  QDW.create driverWallet

getFinancialYearStart :: Seconds -> UTCTime -> Int
getFinancialYearStart timeDiffFromUtc timestamp =
  let localDay = utctDay $ addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) timestamp
      (year, month, _) = toGregorian localDay
   in fromEnum $ if month < 4 then year - 1 else year

makeWalletRunningBalanceLockKey :: Text -> Text
makeWalletRunningBalanceLockKey personId = "WalletRunningBalanceLockKey:" <> personId
