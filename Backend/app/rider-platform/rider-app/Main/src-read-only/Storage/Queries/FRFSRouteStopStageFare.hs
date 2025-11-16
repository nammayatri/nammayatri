{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSRouteStopStageFare where

import qualified Domain.Types.FRFSFarePolicy
import qualified Domain.Types.FRFSRouteStopStageFare
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSRouteStopStageFare as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare] -> m ())
createMany = traverse_ create

findByRouteAndStopCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare))
findByRouteAndStopCode farePolicyId routeCode stopCode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId),
          Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.stopCode $ Se.Eq stopCode
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare))
findByPrimaryKey farePolicyId routeCode stopCode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId),
          Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.stopCode $ Se.Eq stopCode
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare -> m ())
updateByPrimaryKey (Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.stage stage,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId), Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.stopCode $ Se.Eq stopCode]]

instance FromTType' Beam.FRFSRouteStopStageFare Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare where
  fromTType' (Beam.FRFSRouteStopStageFareT {..}) = do
    pure $
      Just
        Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare
          { farePolicyId = Kernel.Types.Id.Id farePolicyId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeCode = routeCode,
            stage = stage,
            stopCode = stopCode,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSRouteStopStageFare Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare where
  toTType' (Domain.Types.FRFSRouteStopStageFare.FRFSRouteStopStageFare {..}) = do
    Beam.FRFSRouteStopStageFareT
      { Beam.farePolicyId = Kernel.Types.Id.getId farePolicyId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeCode = routeCode,
        Beam.stage = stage,
        Beam.stopCode = stopCode,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
