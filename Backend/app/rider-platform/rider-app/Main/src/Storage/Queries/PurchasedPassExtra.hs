{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPassExtra where

import Data.Time hiding (getCurrentTime)
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
import Storage.Queries.OrphanInstances.PurchasedPass ()

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
  istTime <- getLocalCurrentTime (19800 :: Seconds)
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.passTypeId $ Se.Eq (getId passTypeId),
          Se.Is Beam.status $ Se.Eq DPurchasedPass.Active,
          Se.Is Beam.endDate $ Se.GreaterThan (utctDay istTime)
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
  Maybe [DPurchasedPass.StatusType] ->
  Maybe Int ->
  Maybe Int ->
  m [DPurchasedPass.PurchasedPass]
findAllByPersonIdWithFilters personId merchantId mbStatus mbLimit mbOffset = do
  let baseConds = [Se.Is Beam.personId $ Se.Eq (getId personId), Se.Is Beam.merchantId $ Se.Eq (getId merchantId)]
      statusConds = case mbStatus of
        Nothing -> []
        Just s -> [Se.Is Beam.status $ Se.In s]
      conds = baseConds ++ statusConds
  findAllWithOptionsKV conds (Se.Desc Beam.createdAt) mbLimit mbOffset

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

getLastPassNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  m Int
getLastPassNumber = do
  pass <- findAllWithOptionsDb [Se.Is Beam.id $ Se.Not $ Se.Eq ""] (Se.Desc Beam.passNumber) (Just 1) (Just 0)
  return $ case listToMaybe pass of
    Just p -> p.passNumber
    Nothing -> 0
