{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BlackListOrg where

import qualified Domain.Types.BlackListOrg
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BlackListOrg as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BlackListOrg.BlackListOrg -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BlackListOrg.BlackListOrg] -> m ())
createMany = traverse_ create

findBySubscriberIdAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> Kernel.Types.Beckn.Domain.Domain -> m (Maybe Domain.Types.BlackListOrg.BlackListOrg))
findBySubscriberIdAndDomain subscriberId domain = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq (Kernel.Types.Id.getShortId subscriberId), Se.Is Beam.domain $ Se.Eq domain]]

findBySubscriberIdDomainMerchantIdAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> Kernel.Types.Beckn.Domain.Domain -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.BlackListOrg.BlackListOrg))
findBySubscriberIdDomainMerchantIdAndMerchantOperatingCityId subscriberId domain merchantId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.subscriberId $ Se.Eq (Kernel.Types.Id.getShortId subscriberId),
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BlackListOrg.BlackListOrg -> m (Maybe Domain.Types.BlackListOrg.BlackListOrg))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BlackListOrg.BlackListOrg -> m ())
updateByPrimaryKey (Domain.Types.BlackListOrg.BlackListOrg {..}) = do
  updateWithKV
    [ Se.Set Beam.domain domain,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.subscriberId (Kernel.Types.Id.getShortId subscriberId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BlackListOrg Domain.Types.BlackListOrg.BlackListOrg where
  fromTType' (Beam.BlackListOrgT {..}) = do
    pure $
      Just
        Domain.Types.BlackListOrg.BlackListOrg
          { domain = domain,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            subscriberId = Kernel.Types.Id.ShortId subscriberId
          }

instance ToTType' Beam.BlackListOrg Domain.Types.BlackListOrg.BlackListOrg where
  toTType' (Domain.Types.BlackListOrg.BlackListOrg {..}) = do
    Beam.BlackListOrgT
      { Beam.domain = domain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.subscriberId = Kernel.Types.Id.getShortId subscriberId
      }
