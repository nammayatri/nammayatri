{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as DPM
import Data.List.NonEmpty
import qualified Domain.Types.ConditionalCharges as DTAC
import Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude hiding (toList)
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy as BeamFP
import qualified Storage.Beam.FarePolicy.FarePolicyAmbulanceDetailsSlab as BeamFPAD
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as BeamFPSS
import qualified Storage.Queries.ConditionalCharges as QueriesAdditionalCharges
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QueriesDEFB
import qualified Storage.Queries.FarePolicy.FarePolicyAmbulanceDetailsSlab as QueriesFPAD
import qualified Storage.Queries.FarePolicy.FarePolicyInterCityDetails as QueriesFPICD
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails as QueriesFPPD
import qualified Storage.Queries.FarePolicy.FarePolicyRentalDetails as QueriesFPRD
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as QueriesFPSDS

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findById (Id farePolicyId) = findOneWithKV [Se.Is BeamFP.id $ Se.Eq farePolicyId]

update' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FarePolicy -> m ()
update' farePolicy = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamFP.nightShiftStart $ (.nightShiftStart) <$> farePolicy.nightShiftBounds,
      Se.Set BeamFP.nightShiftEnd $ (.nightShiftEnd) <$> farePolicy.nightShiftBounds,
      Se.Set BeamFP.maxAllowedTripDistance $ (.maxAllowedTripDistance) <$> farePolicy.allowedTripDistanceBounds,
      Se.Set BeamFP.minAllowedTripDistance $ (.minAllowedTripDistance) <$> farePolicy.allowedTripDistanceBounds,
      Se.Set BeamFP.govtCharges $ farePolicy.govtCharges,
      Se.Set BeamFP.serviceCharge $ roundToIntegral <$> farePolicy.serviceCharge,
      Se.Set BeamFP.tollCharges $ farePolicy.tollCharges,
      Se.Set BeamFP.petCharges $ farePolicy.petCharges,
      Se.Set BeamFP.businessDiscountPercentage $ farePolicy.businessDiscountPercentage,
      Se.Set BeamFP.priorityCharges $ farePolicy.priorityCharges,
      Se.Set BeamFP.pickupBufferInSecsForNightShiftCal $ farePolicy.pickupBufferInSecsForNightShiftCal,
      Se.Set BeamFP.serviceChargeAmount $ farePolicy.serviceCharge,
      Se.Set BeamFP.currency $ Just farePolicy.currency,
      Se.Set BeamFP.perMinuteRideExtraTimeCharge $ farePolicy.perMinuteRideExtraTimeCharge,
      Se.Set BeamFP.congestionCharge $ farePolicy.congestionChargeMultiplier,
      Se.Set BeamFP.description $ farePolicy.description,
      Se.Set BeamFP.updatedAt now
    ]
    [Se.Is BeamFP.id (Se.Eq $ getId farePolicy.id)]

  case farePolicy.farePolicyDetails of
    ProgressiveDetails fPPD ->
      updateOneWithKV
        [ Se.Set BeamFPPD.baseFare $ roundToIntegral fPPD.baseFare,
          Se.Set BeamFPPD.baseFareAmount $ Just fPPD.baseFare,
          Se.Set BeamFPPD.baseDistance $ fPPD.baseDistance,
          Se.Set BeamFPPD.deadKmFare $ roundToIntegral fPPD.deadKmFare,
          Se.Set BeamFPPD.deadKmFareAmount $ Just fPPD.deadKmFare,
          Se.Set BeamFPPD.currency $ Just fPPD.currency,
          Se.Set BeamFPPD.waitingCharge $ (.waitingCharge) <$> fPPD.waitingChargeInfo,
          Se.Set BeamFPPD.freeWatingTime $ (.freeWaitingTime) <$> fPPD.waitingChargeInfo,
          Se.Set BeamFPPD.nightShiftCharge $ fPPD.nightShiftCharge
        ]
        [Se.Is BeamFPPD.farePolicyId (Se.Eq $ getId farePolicy.id)]
    SlabsDetails (FPSlabsDetails _slabs) -> pure ()
    RentalDetails _ -> pure ()
    InterCityDetails _ -> pure ()
    AmbulanceDetails _ -> pure ()

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FarePolicy -> m ()
create farePolicy = do
  case farePolicy.driverExtraFeeBounds of
    Just driverExtraFeeBounds -> mapM_ (\defb -> QueriesDEFB.create (farePolicy.id, defb)) (toList driverExtraFeeBounds)
    Nothing -> pure ()
  case farePolicy.farePolicyDetails of
    ProgressiveDetails fPPD ->
      QueriesFPPD.create (farePolicy.id, fPPD)
    SlabsDetails fPSD ->
      mapM_ (\fps -> QueriesFPSDS.create (farePolicy.id, fps)) (toList fPSD.slabs)
    AmbulanceDetails _ -> pure () -- can be done with slabs
    RentalDetails fPRD -> do
      QueriesFPRD.create (farePolicy.id, fPRD)
    InterCityDetails fPICD ->
      QueriesFPICD.create (farePolicy.id, fPICD)
  createWithKV farePolicy

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id FarePolicy -> m ()
delete farePolicyId = do
  QueriesDEFB.deleteAll' farePolicyId
  QueriesFPPD.delete farePolicyId
  QueriesFPRD.delete farePolicyId
  QueriesFPICD.delete farePolicyId
  QueriesFPSDS.deleteAll' farePolicyId
  deleteWithKV [Se.Is BeamFP.id $ Se.Eq (getId farePolicyId)]

instance ToTType' BeamFP.FarePolicy FarePolicy where
  toTType' FarePolicy {..} = do
    BeamFP.FarePolicyT
      { BeamFP.id = getId id,
        BeamFP.serviceCharge = roundToIntegral <$> serviceCharge,
        BeamFP.serviceChargeAmount = serviceCharge,
        BeamFP.parkingCharge = parkingCharge,
        BeamFP.perStopCharge = perStopCharge,
        BeamFP.tollCharges = tollCharges,
        BeamFP.priorityCharges = priorityCharges,
        BeamFP.pickupBufferInSecsForNightShiftCal = pickupBufferInSecsForNightShiftCal,
        BeamFP.tipOptions = tipOptions,
        BeamFP.currency = Just currency,
        BeamFP.distanceUnit = Just distanceUnit,
        BeamFP.nightShiftStart = (.nightShiftStart) <$> nightShiftBounds,
        BeamFP.nightShiftEnd = (.nightShiftEnd) <$> nightShiftBounds,
        BeamFP.maxAllowedTripDistance = (.maxAllowedTripDistance) <$> allowedTripDistanceBounds,
        BeamFP.minAllowedTripDistance = (.minAllowedTripDistance) <$> allowedTripDistanceBounds,
        BeamFP.govtCharges = govtCharges,
        BeamFP.perMinuteRideExtraTimeCharge = perMinuteRideExtraTimeCharge,
        BeamFP.congestionCharge = congestionChargeMultiplier,
        BeamFP.perDistanceUnitInsuranceCharge = perDistanceUnitInsuranceCharge,
        BeamFP.cardChargePerDistanceUnitMultiplier = cardCharge >>= (.perDistanceUnitMultiplier),
        BeamFP.fixedCardCharge = cardCharge >>= (.fixed),
        BeamFP.farePolicyType = getFarePolicyType $ FarePolicy {..},
        BeamFP.description = description,
        BeamFP.cancellationFarePolicyId = getId <$> cancellationFarePolicyId,
        BeamFP.platformFeeChargesBy = Just platformFeeChargesBy,
        BeamFP.createdAt = createdAt,
        BeamFP.updatedAt = updatedAt,
        BeamFP.merchantId = getId <$> merchantId,
        BeamFP.merchantOperatingCityId = getId <$> merchantOperatingCityId,
        ..
      }

instance FromTType' BeamFP.FarePolicy Domain.FarePolicy where
  fromTType' farePolicyT = fromTTypeFarePolicy (mkBeamFarePolicyHandler farePolicyT) farePolicyT

data FarePolicyHandler m = FarePolicyHandler
  { findAllDriverExtraFeeBounds :: m [Domain.FullDriverExtraFeeBounds],
    findProgressiveDetails :: m (Maybe Domain.FullFarePolicyProgressiveDetails),
    findAllSlabDetailsSlabs :: m [BeamFPSS.FullFarePolicySlabsDetailsSlab],
    findRentalDetails :: m (Maybe Domain.FullFarePolicyRentalDetails),
    findInterCityDetails :: m (Maybe Domain.FullFarePolicyInterCityDetails),
    findAllAmbulanceDetailsSlabs :: m [BeamFPAD.FullFarePolicyAmbulanceDetailsSlab],
    findAllAdditionalCharges :: m [DTAC.ConditionalCharges]
  }

mkBeamFarePolicyHandler ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  BeamFP.FarePolicy ->
  FarePolicyHandler m
mkBeamFarePolicyHandler BeamFP.FarePolicyT {..} =
  FarePolicyHandler
    { findAllDriverExtraFeeBounds = QueriesDEFB.findAll' (Id id),
      findProgressiveDetails = QueriesFPPD.findById' (Id id),
      findAllSlabDetailsSlabs = QueriesFPSDS.findAll' (Id id),
      findRentalDetails = QueriesFPRD.findById' (Id id),
      findInterCityDetails = QueriesFPICD.findById' (Id id),
      findAllAmbulanceDetailsSlabs = QueriesFPAD.findById' (Id id),
      findAllAdditionalCharges = QueriesAdditionalCharges.findAllByFp id
    }

fromTTypeFarePolicy ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  FarePolicyHandler m ->
  BeamFP.FarePolicy ->
  m (Maybe Domain.FarePolicy)
fromTTypeFarePolicy handler BeamFP.FarePolicyT {..} = do
  fullDEFB <- handler.findAllDriverExtraFeeBounds
  let fDEFB = snd <$> fullDEFB
  mFarePolicyDetails <-
    case farePolicyType of
      Progressive -> do
        mFPPD <- handler.findProgressiveDetails
        case mFPPD of
          Just (_, fPPD) -> return $ Just (ProgressiveDetails fPPD)
          Nothing -> return Nothing
      Slabs -> do
        fullSlabs <- handler.findAllSlabDetailsSlabs
        let slabs = snd <$> fullSlabs
        case nonEmpty slabs of
          Just nESlabs -> return $ Just (SlabsDetails (FPSlabsDetails nESlabs))
          Nothing -> return Nothing
      Rental -> do
        mFPRD <- handler.findRentalDetails
        case mFPRD of
          Just (_, fPRD) -> return $ Just (RentalDetails fPRD)
          Nothing -> return Nothing
      InterCity -> do
        mFPICD <- handler.findInterCityDetails
        case mFPICD of
          Just (_, fPICD) -> return $ Just (InterCityDetails fPICD)
          Nothing -> return Nothing
      Ambulance -> do
        fullAmbulanceSlabs <- handler.findAllAmbulanceDetailsSlabs
        let slabs = snd <$> fullAmbulanceSlabs
        case nonEmpty slabs of
          Just nESlabs -> return $ Just (AmbulanceDetails (FPAmbulanceDetails nESlabs))
          Nothing -> return Nothing
  conditionalCharges <- handler.findAllAdditionalCharges
  case mFarePolicyDetails of
    Just farePolicyDetails -> do
      return $
        Just
          Domain.FarePolicy
            { id = Id id,
              serviceCharge = mkAmountWithDefault serviceChargeAmount <$> serviceCharge,
              parkingCharge = parkingCharge,
              perStopCharge = perStopCharge,
              tollCharges = tollCharges,
              petCharges = petCharges,
              priorityCharges = priorityCharges,
              pickupBufferInSecsForNightShiftCal = pickupBufferInSecsForNightShiftCal,
              tipOptions = tipOptions,
              currency = fromMaybe INR currency,
              distanceUnit = fromMaybe Meter distanceUnit,
              nightShiftBounds = DPM.NightShiftBounds <$> nightShiftStart <*> nightShiftEnd,
              allowedTripDistanceBounds =
                ((,) <$> minAllowedTripDistance <*> maxAllowedTripDistance) <&> \(minAllowedTripDistance', maxAllowedTripDistance') ->
                  AllowedTripDistanceBounds
                    { minAllowedTripDistance = minAllowedTripDistance',
                      maxAllowedTripDistance = maxAllowedTripDistance',
                      distanceUnit = fromMaybe Meter distanceUnit
                    },
              govtCharges = govtCharges,
              driverExtraFeeBounds = nonEmpty fDEFB,
              farePolicyDetails,
              perMinuteRideExtraTimeCharge = perMinuteRideExtraTimeCharge,
              additionalCongestionCharge = 0,
              congestionChargeMultiplier = congestionCharge,
              perDistanceUnitInsuranceCharge = perDistanceUnitInsuranceCharge,
              cardCharge =
                Just $
                  CardCharge
                    { perDistanceUnitMultiplier = cardChargePerDistanceUnitMultiplier,
                      fixed = fixedCardCharge
                    },
              description = description,
              cancellationFarePolicyId = Id <$> cancellationFarePolicyId,
              platformFeeChargesBy = fromMaybe Subscription platformFeeChargesBy,
              createdAt = createdAt,
              updatedAt = updatedAt,
              merchantId = Id <$> merchantId,
              merchantOperatingCityId = Id <$> merchantOperatingCityId,
              conditionalCharges = conditionalCharges,
              ..
            }
    Nothing -> return Nothing
