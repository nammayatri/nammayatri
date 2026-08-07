{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PersonUsageStats
  ( PurchaseEvent (..),
    mkPurchaseEvent,
    recordPurchase,
    reversePurchase,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassType as PassType
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonUsageStats as DPUS
import Kernel.External.Encryption (EncFlow, decrypt)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Utils as SLUtils
import qualified Storage.Queries.PersonUsageStats as QPersonUsageStats

data PurchaseEvent = PurchaseEvent
  { personId :: Id Person.Person,
    staticPersonId :: Maybe Text,
    vehicleType :: Maybe Spec.VehicleCategory,
    vehicleServiceTierType :: Maybe Spec.ServiceTierType,
    productType :: DPUS.FRFSProductType,
    passTypeId :: Maybe (Id PassType.PassType),
    -- | Tickets bought. Nothing for a pass, where only purchaseCount moves.
    quantity :: Maybe Int,
    merchantId :: Id Merchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Show, Generic)

mkPurchaseEvent ::
  (MonadFlow m, EncFlow m r) =>
  Person.Person ->
  Maybe Spec.VehicleCategory ->
  Maybe Spec.ServiceTierType ->
  DPUS.FRFSProductType ->
  Maybe (Id PassType.PassType) ->
  Maybe Int ->
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m PurchaseEvent
mkPurchaseEvent person vehicleType vehicleServiceTierType productType passTypeId quantity merchantId merchantOperatingCityId = do
  staticPersonId <- forM person.mobileNumber $ \encPhone ->
    SLUtils.getPureStaticCustomerId person <$> decrypt encPhone
  pure PurchaseEvent {personId = person.id, ..}

recordPurchase ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  PurchaseEvent ->
  m ()
recordPurchase ev = do
  now <- getCurrentTime
  Redis.withWaitAndLockRedis (lockKey ev.personId) 10 100 $ do
    mbRow <- findRow ev
    case mbRow of
      Just row ->
        QPersonUsageStats.updateCounts
          (liftM2 (+) row.ticketCount ev.quantity)
          (row.purchaseCount + 1)
          now
          row.id
      Nothing -> do
        newId <- generateGUID
        QPersonUsageStats.create
          DPUS.PersonUsageStats
            { id = newId,
              personId = ev.personId,
              staticPersonId = ev.staticPersonId,
              vehicleType = ev.vehicleType,
              vehicleServiceTierType = ev.vehicleServiceTierType,
              productType = ev.productType,
              passTypeId = ev.passTypeId,
              ticketCount = ev.quantity,
              purchaseCount = 1,
              lastPurchasedAt = now,
              merchantId = ev.merchantId,
              merchantOperatingCityId = ev.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }

reversePurchase ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  PurchaseEvent ->
  m ()
reversePurchase ev =
  Redis.withWaitAndLockRedis (lockKey ev.personId) 10 100 $ do
    mbRow <- findRow ev
    case mbRow of
      Nothing ->
        logWarning $ "PersonUsageStats: nothing to reverse for person " <> ev.personId.getId
      Just row ->
        QPersonUsageStats.updateCounts
          (liftM2 (\c q -> max 0 (c - q)) row.ticketCount ev.quantity)
          (max 0 (row.purchaseCount - 1))
          row.lastPurchasedAt
          row.id

findRow ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  PurchaseEvent ->
  m (Maybe DPUS.PersonUsageStats)
findRow ev =
  QPersonUsageStats.findByDimensions
    ev.personId
    ev.vehicleType
    ev.vehicleServiceTierType
    ev.productType
    ev.passTypeId

lockKey :: Id Person.Person -> Text
lockKey personId = "personUsageStats:lock:" <> personId.getId
