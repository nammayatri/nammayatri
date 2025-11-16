{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StationsExtraInformation (module Storage.Queries.StationsExtraInformation, module ReExport) where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.StationsExtraInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Sequelize as Se
import qualified Storage.Beam.StationsExtraInformation as Beam
import Storage.Queries.StationsExtraInformationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StationsExtraInformation.StationsExtraInformation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.StationsExtraInformation.StationsExtraInformation] -> m ())
createMany = traverse_ create

findByStationIdAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.StationsExtraInformation.StationsExtraInformation))
findByStationIdAndCity stationId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.stationId $ Se.Eq stationId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.StationsExtraInformation.StationsExtraInformation -> m (Maybe Domain.Types.StationsExtraInformation.StationsExtraInformation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StationsExtraInformation.StationsExtraInformation -> m ())
updateByPrimaryKey (Domain.Types.StationsExtraInformation.StationsExtraInformation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.stationId stationId,
      Se.Set Beam.suggestedDestinations (Kernel.Utils.Text.encodeToText <$> suggestedDestinations),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
