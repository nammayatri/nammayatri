{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.WhiteListOrg (module Storage.Queries.WhiteListOrg, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.WhiteListOrgExtra as ReExport
import qualified Domain.Types.WhiteListOrg
import qualified Storage.Beam.WhiteListOrg as Beam
import qualified Kernel.Types.Beckn.Domain
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Types.Registry
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WhiteListOrg.WhiteListOrg -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.WhiteListOrg.WhiteListOrg] -> m ())
createMany = traverse_ create
findBySubscriberIdDomainMerchantIdAndMerchantOperatingCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                                                (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> Kernel.Types.Beckn.Domain.Domain -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.WhiteListOrg.WhiteListOrg))
findBySubscriberIdDomainMerchantIdAndMerchantOperatingCityId subscriberId domain merchantId merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq (Kernel.Types.Id.getShortId subscriberId),
                                                                                                                                                Se.Is Beam.domain $ Se.Eq domain,
                                                                                                                                                Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
                                                                                                                                                Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.WhiteListOrg.WhiteListOrg -> m (Maybe Domain.Types.WhiteListOrg.WhiteListOrg))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.WhiteListOrg.WhiteListOrg -> m ())
updateByPrimaryKey (Domain.Types.WhiteListOrg.WhiteListOrg {..}) = do {_now <- getCurrentTime;
                                                                       updateWithKV [Se.Set Beam.domain domain,
                                                                                     Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                     Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                     Se.Set Beam.subscriberId (Kernel.Types.Id.getShortId subscriberId),
                                                                                     Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



