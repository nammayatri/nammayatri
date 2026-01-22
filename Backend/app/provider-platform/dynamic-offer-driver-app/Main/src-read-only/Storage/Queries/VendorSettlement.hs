{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VendorSettlement (module Storage.Queries.VendorSettlement, module ReExport) where

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
import Storage.Queries.VendorSettlementExtra as ReExport

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
    [ Se.Set Beam.runningBalance runningBalance,
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
