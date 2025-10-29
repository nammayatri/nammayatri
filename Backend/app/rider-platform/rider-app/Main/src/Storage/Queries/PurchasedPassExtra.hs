{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPassExtra where

import qualified Domain.Types.Extra.PurchasedPass ()
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Sequelize as Se
import qualified Storage.Beam.PurchasedPass as Beam
import qualified Storage.Queries.PurchasedPass ()

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  m (Maybe DPurchasedPass.PurchasedPass)
findById purchasedPassId = findOneWithKV [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]

findActivePassByPersonIdAndPassTypeId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DPassType.PassType ->
  m (Maybe DPurchasedPass.PurchasedPass)
findActivePassByPersonIdAndPassTypeId personId passTypeId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.passTypeId $ Se.Eq (getId passTypeId),
          Se.Is Beam.status $ Se.Eq DPurchasedPass.Active
        ]
    ]

findAllByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  m [DPurchasedPass.PurchasedPass]
findAllByPersonId personId = do
  findAllWithKV
    [Se.Is Beam.personId $ Se.Eq (getId personId)]

findAllByPersonIdWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Maybe DPurchasedPass.StatusType ->
  Maybe Int ->
  Maybe Int ->
  m [DPurchasedPass.PurchasedPass]
findAllByPersonIdWithFilters personId merchantId mbStatus mbLimit mbOffset = do
  let baseConds = [Se.Is Beam.personId $ Se.Eq (getId personId), Se.Is Beam.merchantId $ Se.Eq (getId merchantId)]
      statusConds = case mbStatus of
        Nothing -> []
        Just s -> [Se.Is Beam.status $ Se.Eq s]
      conds = baseConds ++ statusConds
  findAllWithOptionsKV conds (Se.Desc Beam.createdAt) mbLimit mbOffset

findByShortId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ShortId DPurchasedPass.PurchasedPass ->
  m (Maybe DPurchasedPass.PurchasedPass)
findByShortId shortId = do
  findOneWithKV
    [Se.Is Beam.shortId $ Se.Eq (getShortId shortId)]

findByOrderShortId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ShortId DOrder.PaymentOrder ->
  m (Maybe DPurchasedPass.PurchasedPass)
findByOrderShortId orderShortId = do
  findOneWithKV
    [Se.Is Beam.orderShortId $ Se.Eq (getShortId orderShortId)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DPurchasedPass.StatusType ->
  Id DPurchasedPass.PurchasedPass ->
  m ()
updateStatusById status purchasedPassId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]

incrementUsedCount ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  m ()
incrementUsedCount purchasedPassId = do
  now <- getCurrentTime
  purchasedPass <- findOneWithKV [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]
  case purchasedPass of
    Nothing -> pure ()
    Just pp ->
      updateWithKV
        [ Se.Set Beam.usedCount (pp.usedCount + 1),
          Se.Set Beam.updatedAt now
        ]
        [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]
