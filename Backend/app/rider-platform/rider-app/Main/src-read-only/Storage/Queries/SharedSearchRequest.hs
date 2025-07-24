{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedSearchRequest where

import qualified Domain.Types.SharedSearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SharedSearchRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedSearchRequest.SharedSearchRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedSearchRequest.SharedSearchRequest] -> m ())
createMany = traverse_ create

findActiveRequests :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedSearchRequest.SharedSearchRequestStatus -> m ([Domain.Types.SharedSearchRequest.SharedSearchRequest]))
findActiveRequests status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.SharedSearchRequest.SharedSearchRequestStatus -> Kernel.Types.Id.Id Domain.Types.SharedSearchRequest.SharedSearchRequest -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SharedSearchRequest.SharedSearchRequest -> m (Maybe Domain.Types.SharedSearchRequest.SharedSearchRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedSearchRequest.SharedSearchRequest -> m ())
updateByPrimaryKey (Domain.Types.SharedSearchRequest.SharedSearchRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.distanceUnit ((maxDistance <&> (.unit))),
      Se.Set Beam.maxDistance (((Kernel.Utils.Common.getHighPrecMeters . Kernel.Utils.Common.distanceToHighPrecMeters <$> maxDistance))),
      Se.Set Beam.maxDistanceValue ((Kernel.Utils.Common.distanceToHighPrecDistance <$> (maxDistance <&> (.unit)) <*> maxDistance)),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.searchRequestIds (Kernel.Types.Id.getId <$> searchRequestIds),
      Se.Set Beam.status status,
      Se.Set Beam.currency ((totalCustomerExtraFee <&> (.currency))),
      Se.Set Beam.totalCustomerExtraFee ((totalCustomerExtraFee <&> (.amountInt))),
      Se.Set Beam.totalCustomerExtraFeeAmount ((totalCustomerExtraFee <&> (.amount))),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.waypoints waypoints
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedSearchRequest Domain.Types.SharedSearchRequest.SharedSearchRequest where
  fromTType' (Beam.SharedSearchRequestT {..}) = do
    pure $
      Just
        Domain.Types.SharedSearchRequest.SharedSearchRequest
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            maxDistance = ((Kernel.Utils.Common.mkDistanceWithDefault distanceUnit maxDistanceValue . Kernel.Types.Common.HighPrecMeters <$> maxDistance)),
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            searchRequestIds = Kernel.Types.Id.Id <$> searchRequestIds,
            status = status,
            totalCustomerExtraFee = Kernel.Utils.Common.mkPriceWithDefault totalCustomerExtraFeeAmount currency <$> totalCustomerExtraFee,
            updatedAt = updatedAt,
            validTill = validTill,
            vehicleCategory = vehicleCategory,
            waypoints = waypoints
          }

instance ToTType' Beam.SharedSearchRequest Domain.Types.SharedSearchRequest.SharedSearchRequest where
  toTType' (Domain.Types.SharedSearchRequest.SharedSearchRequest {..}) = do
    Beam.SharedSearchRequestT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.distanceUnit = (maxDistance <&> (.unit)),
        Beam.maxDistance = ((Kernel.Utils.Common.getHighPrecMeters . Kernel.Utils.Common.distanceToHighPrecMeters <$> maxDistance)),
        Beam.maxDistanceValue = (Kernel.Utils.Common.distanceToHighPrecDistance <$> (maxDistance <&> (.unit)) <*> maxDistance),
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.searchRequestIds = Kernel.Types.Id.getId <$> searchRequestIds,
        Beam.status = status,
        Beam.currency = (totalCustomerExtraFee <&> (.currency)),
        Beam.totalCustomerExtraFee = (totalCustomerExtraFee <&> (.amountInt)),
        Beam.totalCustomerExtraFeeAmount = (totalCustomerExtraFee <&> (.amount)),
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.vehicleCategory = vehicleCategory,
        Beam.waypoints = waypoints
      }
