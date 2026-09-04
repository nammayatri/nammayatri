{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareAlertSubscription where

import qualified Domain.Types.FareAlertSubscription
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareAlertSubscription as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareAlertSubscription.FareAlertSubscription -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareAlertSubscription.FareAlertSubscription] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareAlertSubscription.FareAlertSubscription -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.FareAlertSubscription.FareAlertSubscription]))
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FareAlertSubscription.FareAlertSubscription -> m (Maybe Domain.Types.FareAlertSubscription.FareAlertSubscription))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareAlertSubscription.FareAlertSubscription -> m ())
updateByPrimaryKey (Domain.Types.FareAlertSubscription.FareAlertSubscription {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.alertType alertType,
      Se.Set Beam.email email,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.subscribedBy subscribedBy,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FareAlertSubscription Domain.Types.FareAlertSubscription.FareAlertSubscription where
  fromTType' (Beam.FareAlertSubscriptionT {..}) = do
    pure $
      Just
        Domain.Types.FareAlertSubscription.FareAlertSubscription
          { alertType = alertType,
            email = email,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            subscribedBy = subscribedBy,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FareAlertSubscription Domain.Types.FareAlertSubscription.FareAlertSubscription where
  toTType' (Domain.Types.FareAlertSubscription.FareAlertSubscription {..}) = do
    Beam.FareAlertSubscriptionT
      { Beam.alertType = alertType,
        Beam.email = email,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.subscribedBy = subscribedBy,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
