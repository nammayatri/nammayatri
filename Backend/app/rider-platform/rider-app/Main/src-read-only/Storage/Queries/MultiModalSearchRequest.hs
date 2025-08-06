{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MultiModalSearchRequest where

import qualified Data.Text
import qualified Domain.Types.MultiModalSearchRequest
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.MultiModalSearchRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest -> m (Maybe Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest]))
findByRiderId riderId = do findAllWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest -> m (Maybe Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest -> m ())
updateByPrimaryKey (Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.backendAppVersion backendAppVersion,
      Se.Set Beam.backendConfigVersion (Kernel.Utils.Version.versionToText <$> backendConfigVersion),
      Se.Set Beam.clientBundleVersion (Kernel.Utils.Version.versionToText <$> clientBundleVersion),
      Se.Set Beam.clientConfigVersion (Kernel.Utils.Version.versionToText <$> clientConfigVersion),
      Se.Set Beam.clientManufacturer ((clientDevice >>= (.deviceManufacturer))),
      Se.Set Beam.clientModelName ((clientDevice <&> (.deviceModel))),
      Se.Set Beam.clientOsType (clientDevice <&> (.deviceType)),
      Se.Set Beam.clientOsVersion (clientDevice <&> (.deviceVersion)),
      Se.Set Beam.clientId (Kernel.Types.Id.getId <$> clientId),
      Se.Set Beam.clientReactNativeVersion clientReactNativeVersion,
      Se.Set Beam.clientSdkVersion (Kernel.Utils.Version.versionToText <$> clientSdkVersion),
      Se.Set Beam.configInExperimentVersions (Just $ toJSON configInExperimentVersions),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.isDashboardRequest isDashboardRequest,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.recentLocationId (Kernel.Types.Id.getId <$> recentLocationId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MultiModalSearchRequest Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest where
  fromTType' (Beam.MultiModalSearchRequestT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    pure $
      Just
        Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest
          { backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientId = Kernel.Types.Id.Id <$> clientId,
            clientReactNativeVersion = clientReactNativeVersion,
            clientSdkVersion = clientSdkVersion',
            configInExperimentVersions = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< configInExperimentVersions),
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            isDashboardRequest = isDashboardRequest,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            recentLocationId = Kernel.Types.Id.Id <$> recentLocationId,
            riderId = Kernel.Types.Id.Id riderId,
            startTime = startTime,
            validTill = validTill,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MultiModalSearchRequest Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest where
  toTType' (Domain.Types.MultiModalSearchRequest.MultiModalSearchRequest {..}) = do
    Beam.MultiModalSearchRequestT
      { Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = Kernel.Utils.Version.versionToText <$> backendConfigVersion,
        Beam.clientBundleVersion = Kernel.Utils.Version.versionToText <$> clientBundleVersion,
        Beam.clientConfigVersion = Kernel.Utils.Version.versionToText <$> clientConfigVersion,
        Beam.clientManufacturer = (clientDevice >>= (.deviceManufacturer)),
        Beam.clientModelName = (clientDevice <&> (.deviceModel)),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.clientReactNativeVersion = clientReactNativeVersion,
        Beam.clientSdkVersion = Kernel.Utils.Version.versionToText <$> clientSdkVersion,
        Beam.configInExperimentVersions = Just $ toJSON configInExperimentVersions,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isDashboardRequest = isDashboardRequest,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.recentLocationId = Kernel.Types.Id.getId <$> recentLocationId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.startTime = startTime,
        Beam.validTill = validTill,
        Beam.updatedAt = updatedAt
      }
