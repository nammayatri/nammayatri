{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Estimate where

import qualified Domain.Types.Common
import qualified Domain.Types.Estimate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as Beam
import qualified Storage.Cac.FarePolicy
import qualified Storage.Queries.FareParameters

create :: KvDbFlow m r => (Domain.Types.Estimate.Estimate -> m ())
create tbl = do Kernel.Prelude.whenJust tbl.fareParams Storage.Queries.FareParameters.create; createWithKV tbl

createMany :: KvDbFlow m r => ([Domain.Types.Estimate.Estimate] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe Domain.Types.Estimate.Estimate))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe Domain.Types.Estimate.Estimate))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.Estimate.Estimate -> m ())
updateByPrimaryKey (Domain.Types.Estimate.Estimate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.fareParamsId ((Kernel.Types.Id.getId . (.id) <$>) fareParams),
      Se.Set Beam.farePolicyId ((Kernel.Types.Id.getId . (.id) <$>) farePolicy),
      Se.Set Beam.isBlockedRoute isBlockedRoute,
      Se.Set Beam.isCustomerPrefferedSearchRoute isCustomerPrefferedSearchRoute,
      Se.Set Beam.isScheduled (Kernel.Prelude.Just isScheduled),
      Se.Set Beam.maxFare (Kernel.Prelude.roundToIntegral maxFare),
      Se.Set Beam.maxFareAmount (Kernel.Prelude.Just maxFare),
      Se.Set Beam.minFare (Kernel.Prelude.roundToIntegral minFare),
      Se.Set Beam.minFareAmount (Kernel.Prelude.Just minFare),
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.specialLocationTag specialLocationTag,
      Se.Set Beam.tollNames tollNames,
      Se.Set Beam.tripCategory (Kernel.Prelude.Just tripCategory),
      Se.Set Beam.updatedAt (Just _now),
      Se.Set Beam.vehicleVariant vehicleServiceTier,
      Se.Set Beam.vehicleServiceTierName vehicleServiceTierName
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  fromTType' (Beam.EstimateT {..}) = do
    farePolicy' <- maybe (pure Nothing) (Storage.Cac.FarePolicy.findById Nothing . Kernel.Types.Id.Id) farePolicyId
    fareParams' <- maybe (pure Nothing) (Storage.Queries.FareParameters.findById . Kernel.Types.Id.Id) fareParamsId
    pure $
      Just
        Domain.Types.Estimate.Estimate
          { createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            estimatedDistance = estimatedDistance,
            fareParams = fareParams',
            farePolicy = farePolicy',
            id = Kernel.Types.Id.Id id,
            isBlockedRoute = isBlockedRoute,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isScheduled = Kernel.Prelude.fromMaybe Kernel.Prelude.False isScheduled,
            maxFare = Kernel.Types.Common.mkAmountWithDefault maxFareAmount maxFare,
            minFare = Kernel.Types.Common.mkAmountWithDefault minFareAmount minFare,
            requestId = Kernel.Types.Id.Id requestId,
            specialLocationTag = specialLocationTag,
            tollNames = tollNames,
            tripCategory = Kernel.Prelude.fromMaybe (Domain.Types.Common.OneWay Domain.Types.Common.OneWayOnDemandDynamicOffer) tripCategory,
            updatedAt = Kernel.Prelude.fromMaybe createdAt updatedAt,
            vehicleServiceTier = vehicleVariant,
            vehicleServiceTierName = vehicleServiceTierName
          }

instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  toTType' (Domain.Types.Estimate.Estimate {..}) = do
    Beam.EstimateT
      { Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.estimatedDistance = estimatedDistance,
        Beam.fareParamsId = (Kernel.Types.Id.getId . (.id) <$>) fareParams,
        Beam.farePolicyId = (Kernel.Types.Id.getId . (.id) <$>) farePolicy,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isScheduled = Kernel.Prelude.Just isScheduled,
        Beam.maxFare = Kernel.Prelude.roundToIntegral maxFare,
        Beam.maxFareAmount = Kernel.Prelude.Just maxFare,
        Beam.minFare = Kernel.Prelude.roundToIntegral minFare,
        Beam.minFareAmount = Kernel.Prelude.Just minFare,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.specialLocationTag = specialLocationTag,
        Beam.tollNames = tollNames,
        Beam.tripCategory = Kernel.Prelude.Just tripCategory,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.vehicleVariant = vehicleServiceTier,
        Beam.vehicleServiceTierName = vehicleServiceTierName
      }
