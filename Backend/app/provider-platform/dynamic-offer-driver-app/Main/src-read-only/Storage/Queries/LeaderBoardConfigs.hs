{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LeaderBoardConfigs where

import qualified Domain.Types.LeaderBoardConfigs
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LeaderBoardConfigs as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findLeaderBoardConfigbyType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.LeaderBoardConfigs.LeaderBoardType -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs))
findLeaderBoardConfigbyType leaderBoardType merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.leaderBoardType $ Se.Eq leaderBoardType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs -> m (Maybe Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs -> m ())
updateByPrimaryKey (Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isEnabled isEnabled,
      Se.Set Beam.leaderBoardExpiry leaderBoardExpiry,
      Se.Set Beam.leaderBoardLengthLimit leaderBoardLengthLimit,
      Se.Set Beam.leaderBoardType leaderBoardType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.numberOfSets numberOfSets,
      Se.Set Beam.useOperatingCityBasedLeaderBoard useOperatingCityBasedLeaderBoard,
      Se.Set Beam.zScoreBase zScoreBase,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.LeaderBoardConfigs Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs where
  fromTType' (Beam.LeaderBoardConfigsT {..}) = do
    pure $
      Just
        Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs
          { id = Kernel.Types.Id.Id id,
            isEnabled = isEnabled,
            leaderBoardExpiry = leaderBoardExpiry,
            leaderBoardLengthLimit = leaderBoardLengthLimit,
            leaderBoardType = leaderBoardType,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            numberOfSets = numberOfSets,
            useOperatingCityBasedLeaderBoard = useOperatingCityBasedLeaderBoard,
            zScoreBase = zScoreBase,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.LeaderBoardConfigs Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs where
  toTType' (Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs {..}) = do
    Beam.LeaderBoardConfigsT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.isEnabled = isEnabled,
        Beam.leaderBoardExpiry = leaderBoardExpiry,
        Beam.leaderBoardLengthLimit = leaderBoardLengthLimit,
        Beam.leaderBoardType = leaderBoardType,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.numberOfSets = numberOfSets,
        Beam.useOperatingCityBasedLeaderBoard = useOperatingCityBasedLeaderBoard,
        Beam.zScoreBase = zScoreBase,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
