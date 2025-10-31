{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VendorSettlement where

import qualified Data.Text
import qualified Domain.Types.VendorSettlement
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VendorSettlement as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorSettlement.VendorSettlement -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VendorSettlement.VendorSettlement] -> m ())
createMany = traverse_ create

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Data.Text.Text -> Kernel.Types.Id.Id Domain.Types.VendorSettlement.VendorSettlement -> Data.Text.Text -> m (Maybe Domain.Types.VendorSettlement.VendorSettlement))
findByPrimaryKey fromVendorId id toVendorId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fromVendorId $ Se.Eq fromVendorId,
          Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.toVendorId $ Se.Eq toVendorId
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorSettlement.VendorSettlement -> m ())
updateByPrimaryKey (Domain.Types.VendorSettlement.VendorSettlement {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.runningBalance runningBalance,
      Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.settlementMode settlementMode,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [ Se.And
        [ Se.Is Beam.fromVendorId $ Se.Eq fromVendorId,
          Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.toVendorId $ Se.Eq toVendorId
        ]
    ]

instance FromTType' Beam.VendorSettlement Domain.Types.VendorSettlement.VendorSettlement where
  fromTType' (Beam.VendorSettlementT {..}) = do
    pure $
      Just
        Domain.Types.VendorSettlement.VendorSettlement
          { createdAt = createdAt,
            fromVendorId = fromVendorId,
            id = Kernel.Types.Id.Id id,
            runningBalance = runningBalance,
            settlementDate = settlementDate,
            settlementMode = settlementMode,
            status = status,
            toVendorId = toVendorId,
            updatedAt = updatedAt,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.VendorSettlement Domain.Types.VendorSettlement.VendorSettlement where
  toTType' (Domain.Types.VendorSettlement.VendorSettlement {..}) = do
    Beam.VendorSettlementT
      { Beam.createdAt = createdAt,
        Beam.fromVendorId = fromVendorId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.runningBalance = runningBalance,
        Beam.settlementDate = settlementDate,
        Beam.settlementMode = settlementMode,
        Beam.status = status,
        Beam.toVendorId = toVendorId,
        Beam.updatedAt = updatedAt,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
