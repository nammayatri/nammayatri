{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EndOtpConfig where

import qualified Domain.Types.EndOtpConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EndOtpConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EndOtpConfig.EndOtpConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.EndOtpConfig.EndOtpConfig] -> m ())
createMany = traverse_ create

findByMerchantOpCityIdAndTripCategoryAndTripMode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.EndOtpConfig.EndOtpConfig))
findByMerchantOpCityIdAndTripCategoryAndTripMode merchantOperatingCityId tripCategory tripMode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.tripMode $ Se.Eq tripMode
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.EndOtpConfig.EndOtpConfig))
findByPrimaryKey merchantOperatingCityId tripCategory tripMode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.tripMode $ Se.Eq tripMode
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EndOtpConfig.EndOtpConfig -> m ())
updateByPrimaryKey (Domain.Types.EndOtpConfig.EndOtpConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isEndOtpRequired isEndOtpRequired,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.tripMode $ Se.Eq tripMode
        ]
    ]

instance FromTType' Beam.EndOtpConfig Domain.Types.EndOtpConfig.EndOtpConfig where
  fromTType' (Beam.EndOtpConfigT {..}) = do
    pure $
      Just
        Domain.Types.EndOtpConfig.EndOtpConfig
          { createdAt = createdAt,
            isEndOtpRequired = isEndOtpRequired,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            tripCategory = tripCategory,
            tripMode = tripMode,
            updatedAt = updatedAt
          }

instance ToTType' Beam.EndOtpConfig Domain.Types.EndOtpConfig.EndOtpConfig where
  toTType' (Domain.Types.EndOtpConfig.EndOtpConfig {..}) = do
    Beam.EndOtpConfigT
      { Beam.createdAt = createdAt,
        Beam.isEndOtpRequired = isEndOtpRequired,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.tripCategory = tripCategory,
        Beam.tripMode = tripMode,
        Beam.updatedAt = updatedAt
      }
