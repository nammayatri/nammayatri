module Storage.Queries.MerchantServiceConfigExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MerchantServiceConfig
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantServiceConfig as BeamMSC
import Storage.Queries.OrphanInstances.MerchantServiceConfig ()
import Storage.Queries.Transformers.MerchantServiceConfig

findByServiceAndCity ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  ServiceName ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe MerchantServiceConfig)
findByServiceAndCity serviceName merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMSC.serviceName $ Se.Eq serviceName,
          Se.Is BeamMSC.merchantOperatingCityId $ Se.Eq (Just merchantOperatingCityId.getId)
        ]
    ]

findAllMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [MerchantServiceConfig]
findAllMerchantOpCityId (Id merchantOperatingCityId) = findAllWithKV [Se.Is BeamMSC.merchantOperatingCityId $ Se.Eq $ Just merchantOperatingCityId]

-- NOTE (reviewer, remove before merge): new read-only query, bulk-only. Its one caller is
--   Storage.CachedQueries.Merchant.MerchantServiceConfig.findAllHdfcCbxPayoutConfigs (startup), always with an HdfcCbx
--   service name. No existing query changed, so no Juspay/Stripe effect.

-- | Every city's config for one service. Used at startup to build the mutually-authenticated
--   HDFC CBX managers, which need the certificate material of every city that has one.
findAllByServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => ServiceName -> m [MerchantServiceConfig]
findAllByServiceName serviceName = findAllWithKV [Se.Is BeamMSC.serviceName $ Se.Eq serviceName]

findOne :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => ServiceName -> m (Maybe MerchantServiceConfig)
findOne serviceName = findAllWithOptionsKV [Se.Is BeamMSC.serviceName $ Se.Eq serviceName] (Se.Desc BeamMSC.createdAt) (Just 1) Nothing <&> listToMaybe

upsertMerchantServiceConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => MerchantServiceConfig -> Id DMOC.MerchantOperatingCity -> m ()
upsertMerchantServiceConfig merchantServiceConfig opCity = do
  now <- getCurrentTime
  let _serviceName = getServiceName merchantServiceConfig.serviceConfig
      configJSON = getConfigJSON merchantServiceConfig.serviceConfig
  res <- findByServiceAndCity _serviceName opCity
  if isJust res
    then
      updateWithKV
        [ Se.Set BeamMSC.configJSON configJSON,
          Se.Set BeamMSC.updatedAt now
        ]
        [ Se.And
            [ Se.Is BeamMSC.merchantId $ Se.Eq $ getId merchantServiceConfig.merchantId,
              Se.Is BeamMSC.merchantOperatingCityId $ Se.Eq $ Just $ getId opCity,
              Se.Is BeamMSC.serviceName $ Se.Eq _serviceName
            ]
        ]
    else createWithKV merchantServiceConfig
