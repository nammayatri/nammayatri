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
import qualified Data.Map as M
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Dashboard.Common as DCommon
import Domain.Types.DriverWallet
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Plan as DP
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Domain.Types.VehicleVariant as Vehicle
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.Finance.Prepaid
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.BookingExtra as QBookingE
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSPE
import qualified Storage.Queries.Vehicle as QVehicle

getSubscriptionTransactions ::
  (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
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
  m SubscriptionTransactionResponse
getSubscriptionTransactions (mbDriverId, _, merchantOperatingCityId) mbFrom mbLimit mbMaxAmount mbMinAmount mbOffset mbStatus mbTo = do
  driverId' <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  driver <- QP.findById driverId' >>= fromMaybeM (PersonNotFound driverId'.getId)
  now <- getCurrentTime
  let fromDate = fromMaybe (UTCTime (utctDay now) 0) mbFrom
      toDate = fromMaybe now mbTo
      limit = min maxLimit . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset
      isFleetOwner = DCommon.checkFleetOwnerRole driver.role
      counterpartyType = if isFleetOwner then counterpartyFleetOwner else counterpartyDriver
      ownerType = if isFleetOwner then DSP.FLEET_OWNER else DSP.DRIVER
  tc <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  let isolationEnabled = fromMaybe False tc.subscriptionConfig.vehicleCategoryScopedPrepaidEnabled
  mbVehicleCategory <-
    if isolationEnabled
      then do
        mVehicle <- QVehicle.findById (cast driverId')
        pure $ fmap (\v -> fromMaybe (Vehicle.castVehicleVariantToVehicleCategory v.variant) v.category) mVehicle
      else pure Nothing
  mbAccount <- getPrepaidAccountByOwner counterpartyType driverId'.getId mbVehicleCategory
  activeSubscriptions <- QSPE.findAllActiveByOwnerAndServiceName driverId'.getId ownerType DP.PREPAID_SUBSCRIPTION mbVehicleCategory
  let currentBalance = sum $ map (.planRideCredit) activeSubscriptions
  case mbAccount of
    Nothing -> pure (emptyResponse currentBalance)
    Just acc -> do
      accountEntries <- filter entryMovedMoney <$> getEntriesByAccount acc.id
      let referenceTypes = [prepaidRideDebitReferenceType, subscriptionCreditReferenceType, expiryCreditTransferReferenceType]
          entries = filter (matchesFilters referenceTypes fromDate toDate) accountEntries
          downSorted = sortOn (Down . effectiveAt) entries
          rideEntries = filter (\e -> e.referenceType == prepaidRideDebitReferenceType) entries
          planEntries = filter (\e -> e.referenceType == subscriptionCreditReferenceType) entries
          expiryEntries = filter (\e -> e.referenceType == expiryCreditTransferReferenceType) entries
          paged = take limit . drop offset $ downSorted
          rideEarning = sum $ map (.amount) rideEntries
          planPurchased = sum $ map (.amount) planEntries
          expiryDeduction = sum $ map (.amount) expiryEntries
          balanceMoves = sortOn fst $ mapMaybe (\e -> (effectiveAt e,) <$> balancesOf acc.id e) accountEntries
          startingBalance = computeStartingBalance fromDate balanceMoves
          finalBalance = computeFinalBalance toDate startingBalance balanceMoves
      -- Enrich ride entries with booking locations
      let bookingIds = mapMaybe (\e -> if e.referenceType == prepaidRideDebitReferenceType then Just (Id e.referenceId) else Nothing) paged
      bookings <- if null bookingIds then pure [] else QBookingE.findByIds bookingIds
      let bookingMap = M.fromList $ map (\b -> (b.id, (b.fromLocation, b.toLocation))) bookings
      entities <- forM paged $ \entry -> do
        transactionType <-
          if
              | entry.referenceType == prepaidRideDebitReferenceType -> pure RIDE
              | entry.referenceType == subscriptionCreditReferenceType -> pure PLAN_PURCHASE
              | entry.referenceType == expiryCreditTransferReferenceType -> pure SUBSCRIPTION_EXPIRY
              | otherwise -> throwError $ InternalError $ "Unknown reference type: " <> show entry.referenceType
        let (fromLocation, toLocation) =
              if entry.referenceType == prepaidRideDebitReferenceType
                then case M.lookup (Id entry.referenceId) bookingMap of
                  Just (fromLoc, toLoc) -> (Just fromLoc, toLoc)
                  Nothing -> (Nothing, Nothing)
                else (Nothing, Nothing)
        pure
          SubscriptionTransactionEntity
            { driverId = driverId'.getId,
              entityId = Just entry.referenceId,
              transactionType,
              amount = entry.amount,
              status = entry.status,
              fromLocation,
              toLocation,
              createdAt = effectiveAt entry,
              updatedAt = entry.updatedAt
            }
      pure
        SubscriptionTransactionResponse
          { startingBalance,
            finalBalance,
            currentBalance,
            rideEarning,
            planPurchased,
            expiryDeduction,
            entities
          }
  where
    maxLimit = 500
    emptyResponse currentBalance =
      SubscriptionTransactionResponse
        { startingBalance = 0,
          finalBalance = 0,
          currentBalance,
          rideEarning = 0,
          planPurchased = 0,
          expiryDeduction = 0,
          entities = []
        }
    effectiveAt :: LedgerEntry -> UTCTime
    effectiveAt entry = fromMaybe entry.timestamp entry.settledAt

    matchesFilters :: [Text] -> UTCTime -> UTCTime -> LedgerEntry -> Bool
    matchesFilters referenceTypes rangeStart rangeEnd entry =
      and
        [ entry.referenceType `elem` referenceTypes,
          effectiveAt entry >= rangeStart,
          effectiveAt entry <= rangeEnd,
          maybe True (\minAmt -> entry.amount >= minAmt) mbMinAmount,
          maybe True (\maxAmt -> entry.amount <= maxAmt) mbMaxAmount,
          maybe True (\st -> entry.status == st) mbStatus
        ]

    balancesOf :: Id Account -> LedgerEntry -> Maybe (HighPrecMoney, HighPrecMoney)
    balancesOf accountId entry
      | entry.fromAccountId == accountId = (,) <$> entry.fromStartingBalance <*> entry.fromEndingBalance
      | otherwise = (,) <$> entry.toStartingBalance <*> entry.toEndingBalance

    computeStartingBalance :: UTCTime -> [(UTCTime, (HighPrecMoney, HighPrecMoney))] -> HighPrecMoney
    computeStartingBalance rangeStart moves =
      case (reverse (filter ((< rangeStart) . fst) moves), filter ((>= rangeStart) . fst) moves) of
        ((_, (_, closing)) : _, _) -> closing
        ([], (_, (opening, _)) : _) -> opening
        _ -> 0

    computeFinalBalance :: UTCTime -> HighPrecMoney -> [(UTCTime, (HighPrecMoney, HighPrecMoney))] -> HighPrecMoney
    computeFinalBalance rangeEnd openingBalance moves =
      case reverse (filter ((<= rangeEnd) . fst) moves) of
        (_, (_, closing)) : _ -> closing
        [] -> openingBalance
