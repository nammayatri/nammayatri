{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VendorSettlementExtra where

import qualified Domain.Types.VendorSettlement as DVS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VendorSettlement as Beam
import Storage.Queries.OrphanInstances.VendorSettlement

-- Extra code goes here --

-- Find latest SettleInNext entry for a vendor pair
-- This is used to fetch the previous carry-forward balance when calculating running balance
findLatestSettleInNextByVendor ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text -> -- fromVendorId
  Text -> -- toVendorId
  m (Maybe DVS.VendorSettlement)
findLatestSettleInNextByVendor fromVendorId toVendorId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.fromVendorId $ Se.Eq fromVendorId,
          Se.Is Beam.toVendorId $ Se.Eq toVendorId,
          Se.Is Beam.status $ Se.Eq DVS.SettleInNext
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

-- Update all SettleInNext entries to Settled for a vendor pair
-- This is called when a final settlement is made to mark all previous carry-forward entries as resolved
updateAllSettleInNextToSettled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text -> -- fromVendorId
  Text -> -- toVendorId
  UTCTime -> -- settlementDate
  m ()
updateAllSettleInNextToSettled fromVendorId toVendorId settlementDate = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status DVS.Settled,
      Se.Set Beam.settlementDate (Just settlementDate),
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.fromVendorId $ Se.Eq fromVendorId,
          Se.Is Beam.toVendorId $ Se.Eq toVendorId,
          Se.Is Beam.status $ Se.Eq DVS.SettleInNext
        ]
    ]

-- Update settlement status by ID
updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DVS.VendorSettlement ->
  DVS.VendorSettlementStatus ->
  UTCTime ->
  m ()
updateStatus settlementId status now =
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId settlementId)]

-- Update settlement date by ID
updateSettlementDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DVS.VendorSettlement ->
  Maybe UTCTime ->
  m ()
updateSettlementDate settlementId settlementDate = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId settlementId)]
