{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LeaderBoardConfigs where

import qualified Domain.Types.LeaderBoardConfigs
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LeaderBoardConfigs as Beam

create :: KvDbFlow m r => (Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs])
findAllByMerchantOpCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findLeaderBoardConfigbyType ::
  KvDbFlow m r =>
  (Domain.Types.LeaderBoardConfigs.LeaderBoardType -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs))
findLeaderBoardConfigbyType leaderBoardType (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.leaderBoardType $ Se.Eq leaderBoardType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
        ]
    ]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs -> m (Maybe Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.LeaderBoardConfigs.LeaderBoardConfigs -> m ())
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
      Se.Set Beam.createdAt createdAt,
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
