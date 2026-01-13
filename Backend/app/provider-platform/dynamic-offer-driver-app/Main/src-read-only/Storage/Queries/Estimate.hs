{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Estimate where

import qualified Domain.Types.Common
import qualified Domain.Types.Estimate
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as Beam
import qualified Storage.Cac.FarePolicy
import qualified Storage.Queries.FareParameters
import qualified Storage.Queries.Transformers.Estimate

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Estimate.Estimate -> m ())
create tbl = do Kernel.Prelude.whenJust tbl.fareParams Storage.Queries.FareParameters.create; createWithKV tbl

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Estimate.Estimate] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe Domain.Types.Estimate.Estimate))
findById id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findEligibleForCabUpgrade :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Prelude.Bool -> m [Domain.Types.Estimate.Estimate])
findEligibleForCabUpgrade requestId eligibleForUpgrade = do
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is Beam.requestId $ Se.Eq (Kernel.Types.Id.getId requestId),
          Se.Is Beam.eligibleForUpgrade $ Se.Eq (Kernel.Prelude.Just eligibleForUpgrade)
        ]
    ]
    Nothing

updateSupplyDemandRatioByReqIdAndServiceTier ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Domain.Types.Common.ServiceTierType -> m ())
updateSupplyDemandRatioByReqIdAndServiceTier supplyDemandRatioFromLoc supplyDemandRatioToLoc requestId vehicleServiceTier = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.supplyDemandRatioFromLoc supplyDemandRatioFromLoc,
      Se.Set Beam.supplyDemandRatioToLoc supplyDemandRatioToLoc,
      Se.Set Beam.updatedAt (Just _now)
    ]
    [ Se.And
        [ Se.Is Beam.requestId $ Se.Eq (Kernel.Types.Id.getId requestId),
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleServiceTier
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m (Maybe Domain.Types.Estimate.Estimate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Estimate.Estimate -> m ())
updateByPrimaryKey (Domain.Types.Estimate.Estimate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.businessDiscount businessDiscount,
      Se.Set Beam.congestionMultiplier congestionMultiplier,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.dpVersion dpVersion,
      Se.Set Beam.driverExtraFeeDefaultStepFee (driverExtraFeeBounds <&> (.defaultStepFee)),
      Se.Set Beam.driverExtraFeeDistanceUnit (driverExtraFeeBounds <&> (.distanceUnit)),
      Se.Set Beam.driverExtraFeeMaxFee (driverExtraFeeBounds <&> (.maxFee)),
      Se.Set Beam.driverExtraFeeMinFee (driverExtraFeeBounds <&> (.minFee)),
      Se.Set Beam.driverExtraFeeStartDistance (driverExtraFeeBounds <&> (.startDistance)),
      Se.Set Beam.driverExtraFeeStepFee (driverExtraFeeBounds <&> (.stepFee)),
      Se.Set Beam.eligibleForUpgrade (Kernel.Prelude.Just eligibleForUpgrade),
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.fareParamsId ((Kernel.Types.Id.getId . (.id) <$>) fareParams),
      Se.Set Beam.farePolicyId ((Kernel.Types.Id.getId . (.id) <$>) farePolicy),
      Se.Set Beam.fromLocGeohash fromLocGeohash,
      Se.Set Beam.isBlockedRoute isBlockedRoute,
      Se.Set Beam.isCustomerPrefferedSearchRoute isCustomerPrefferedSearchRoute,
      Se.Set Beam.isScheduled (Kernel.Prelude.Just isScheduled),
      Se.Set Beam.maxFare (Kernel.Prelude.roundToIntegral maxFare),
      Se.Set Beam.maxFareAmount (Kernel.Prelude.Just maxFare),
      Se.Set Beam.mbActualQARCity mbActualQARCity,
      Se.Set Beam.mbActualQARCityPast mbActualQARCityPast,
      Se.Set Beam.mbActualQARFromLocGeohash mbActualQARFromLocGeohash,
      Se.Set Beam.mbActualQARFromLocGeohashDistance mbActualQARFromLocGeohashDistance,
      Se.Set Beam.mbActualQARFromLocGeohashDistancePast mbActualQARFromLocGeohashDistancePast,
      Se.Set Beam.mbActualQARFromLocGeohashPast mbActualQARFromLocGeohashPast,
      Se.Set Beam.mbCongestionCity mbCongestionCity,
      Se.Set Beam.mbCongestionCityPast mbCongestionCityPast,
      Se.Set Beam.mbCongestionFromLocGeohash mbCongestionFromLocGeohash,
      Se.Set Beam.mbCongestionFromLocGeohashDistance mbCongestionFromLocGeohashDistance,
      Se.Set Beam.mbCongestionFromLocGeohashDistancePast mbCongestionFromLocGeohashDistancePast,
      Se.Set Beam.mbCongestionFromLocGeohashPast mbCongestionFromLocGeohashPast,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.minFare (Kernel.Prelude.roundToIntegral minFare),
      Se.Set Beam.minFareAmount (Kernel.Prelude.Just minFare),
      Se.Set Beam.personalDiscount personalDiscount,
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.smartTipReason smartTipReason,
      Se.Set Beam.smartTipSuggestion smartTipSuggestion,
      Se.Set Beam.specialLocationTag specialLocationTag,
      Se.Set Beam.supplyDemandRatioFromLoc supplyDemandRatioFromLoc,
      Se.Set Beam.supplyDemandRatioToLoc supplyDemandRatioToLoc,
      Se.Set Beam.tipOptions tipOptions,
      Se.Set Beam.tollIds tollIds,
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
          { businessDiscount = businessDiscount,
            congestionMultiplier = congestionMultiplier,
            createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            dpVersion = dpVersion,
            driverExtraFeeBounds = Storage.Queries.Transformers.Estimate.mkDriverExtraFeeBounds driverExtraFeeStartDistance driverExtraFeeDistanceUnit driverExtraFeeStepFee driverExtraFeeDefaultStepFee driverExtraFeeMinFee driverExtraFeeMaxFee,
            eligibleForUpgrade = Kernel.Prelude.fromMaybe False eligibleForUpgrade,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            fareParams = fareParams',
            farePolicy = farePolicy',
            fromLocGeohash = fromLocGeohash,
            id = Kernel.Types.Id.Id id,
            isBlockedRoute = isBlockedRoute,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isScheduled = Kernel.Prelude.fromMaybe Kernel.Prelude.False isScheduled,
            maxFare = Kernel.Types.Common.mkAmountWithDefault maxFareAmount maxFare,
            mbActualQARCity = mbActualQARCity,
            mbActualQARCityPast = mbActualQARCityPast,
            mbActualQARFromLocGeohash = mbActualQARFromLocGeohash,
            mbActualQARFromLocGeohashDistance = mbActualQARFromLocGeohashDistance,
            mbActualQARFromLocGeohashDistancePast = mbActualQARFromLocGeohashDistancePast,
            mbActualQARFromLocGeohashPast = mbActualQARFromLocGeohashPast,
            mbCongestionCity = mbCongestionCity,
            mbCongestionCityPast = mbCongestionCityPast,
            mbCongestionFromLocGeohash = mbCongestionFromLocGeohash,
            mbCongestionFromLocGeohashDistance = mbCongestionFromLocGeohashDistance,
            mbCongestionFromLocGeohashDistancePast = mbCongestionFromLocGeohashDistancePast,
            mbCongestionFromLocGeohashPast = mbCongestionFromLocGeohashPast,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            minFare = Kernel.Types.Common.mkAmountWithDefault minFareAmount minFare,
            personalDiscount = personalDiscount,
            requestId = Kernel.Types.Id.Id requestId,
            smartTipReason = smartTipReason,
            smartTipSuggestion = smartTipSuggestion,
            specialLocationTag = specialLocationTag,
            supplyDemandRatioFromLoc = supplyDemandRatioFromLoc,
            supplyDemandRatioToLoc = supplyDemandRatioToLoc,
            tipOptions = tipOptions,
            tollIds = tollIds,
            tollNames = tollNames,
            tripCategory = Kernel.Prelude.fromMaybe (Domain.Types.Common.OneWay Domain.Types.Common.OneWayOnDemandDynamicOffer) tripCategory,
            updatedAt = Kernel.Prelude.fromMaybe createdAt updatedAt,
            vehicleServiceTier = vehicleVariant,
            vehicleServiceTierName = vehicleServiceTierName
          }

instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  toTType' (Domain.Types.Estimate.Estimate {..}) = do
    Beam.EstimateT
      { Beam.businessDiscount = businessDiscount,
        Beam.congestionMultiplier = congestionMultiplier,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.dpVersion = dpVersion,
        Beam.driverExtraFeeDefaultStepFee = driverExtraFeeBounds <&> (.defaultStepFee),
        Beam.driverExtraFeeDistanceUnit = driverExtraFeeBounds <&> (.distanceUnit),
        Beam.driverExtraFeeMaxFee = driverExtraFeeBounds <&> (.maxFee),
        Beam.driverExtraFeeMinFee = driverExtraFeeBounds <&> (.minFee),
        Beam.driverExtraFeeStartDistance = driverExtraFeeBounds <&> (.startDistance),
        Beam.driverExtraFeeStepFee = driverExtraFeeBounds <&> (.stepFee),
        Beam.eligibleForUpgrade = Kernel.Prelude.Just eligibleForUpgrade,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.fareParamsId = (Kernel.Types.Id.getId . (.id) <$>) fareParams,
        Beam.farePolicyId = (Kernel.Types.Id.getId . (.id) <$>) farePolicy,
        Beam.fromLocGeohash = fromLocGeohash,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isScheduled = Kernel.Prelude.Just isScheduled,
        Beam.maxFare = Kernel.Prelude.roundToIntegral maxFare,
        Beam.maxFareAmount = Kernel.Prelude.Just maxFare,
        Beam.mbActualQARCity = mbActualQARCity,
        Beam.mbActualQARCityPast = mbActualQARCityPast,
        Beam.mbActualQARFromLocGeohash = mbActualQARFromLocGeohash,
        Beam.mbActualQARFromLocGeohashDistance = mbActualQARFromLocGeohashDistance,
        Beam.mbActualQARFromLocGeohashDistancePast = mbActualQARFromLocGeohashDistancePast,
        Beam.mbActualQARFromLocGeohashPast = mbActualQARFromLocGeohashPast,
        Beam.mbCongestionCity = mbCongestionCity,
        Beam.mbCongestionCityPast = mbCongestionCityPast,
        Beam.mbCongestionFromLocGeohash = mbCongestionFromLocGeohash,
        Beam.mbCongestionFromLocGeohashDistance = mbCongestionFromLocGeohashDistance,
        Beam.mbCongestionFromLocGeohashDistancePast = mbCongestionFromLocGeohashDistancePast,
        Beam.mbCongestionFromLocGeohashPast = mbCongestionFromLocGeohashPast,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.minFare = Kernel.Prelude.roundToIntegral minFare,
        Beam.minFareAmount = Kernel.Prelude.Just minFare,
        Beam.personalDiscount = personalDiscount,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.smartTipReason = smartTipReason,
        Beam.smartTipSuggestion = smartTipSuggestion,
        Beam.specialLocationTag = specialLocationTag,
        Beam.supplyDemandRatioFromLoc = supplyDemandRatioFromLoc,
        Beam.supplyDemandRatioToLoc = supplyDemandRatioToLoc,
        Beam.tipOptions = tipOptions,
        Beam.tollIds = tollIds,
        Beam.tollNames = tollNames,
        Beam.tripCategory = Kernel.Prelude.Just tripCategory,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.vehicleVariant = vehicleServiceTier,
        Beam.vehicleServiceTierName = vehicleServiceTierName
      }
