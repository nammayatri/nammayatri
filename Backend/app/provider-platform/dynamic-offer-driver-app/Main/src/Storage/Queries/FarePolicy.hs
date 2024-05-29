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

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
import Data.List.NonEmpty
import Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude hiding (toList)
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy as BeamFP
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as BeamFPSS
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QueriesDEFB
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

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FarePolicy -> m ()
create farePolicy = do
  case farePolicy.driverExtraFeeBounds of
    Just driverExtraFeeBounds -> mapM_ (\defb -> QueriesDEFB.create (farePolicy.id, defb)) (toList driverExtraFeeBounds)
    Nothing -> pure ()
  case farePolicy.farePolicyDetails of
    ProgressiveDetails fPPD ->
      QueriesFPPD.create (farePolicy.id, fPPD)
    SlabsDetails _ -> pure () -- will do later :(
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
        BeamFP.tollCharges = tollCharges,
        BeamFP.currency = Just currency,
        BeamFP.nightShiftStart = (.nightShiftStart) <$> nightShiftBounds,
        BeamFP.nightShiftEnd = (.nightShiftEnd) <$> nightShiftBounds,
        BeamFP.maxAllowedTripDistance = (.maxAllowedTripDistance) <$> allowedTripDistanceBounds,
        BeamFP.minAllowedTripDistance = (.minAllowedTripDistance) <$> allowedTripDistanceBounds,
        BeamFP.govtCharges = govtCharges,
        BeamFP.perMinuteRideExtraTimeCharge = perMinuteRideExtraTimeCharge,
        BeamFP.congestionCharge = congestionChargeMultiplier,
        BeamFP.farePolicyType = getFarePolicyType $ FarePolicy {..},
        BeamFP.description = description,
        BeamFP.createdAt = createdAt,
        BeamFP.updatedAt = updatedAt
      }

instance FromTType' BeamFP.FarePolicy Domain.FarePolicy where
  fromTType' farePolicyT = fromTTypeFarePolicy (mkBeamFarePolicyHandler farePolicyT) farePolicyT

data FarePolicyHandler m = FarePolicyHandler
  { findAllDriverExtraFeeBounds :: m [Domain.FullDriverExtraFeeBounds],
    findProgressiveDetails :: m (Maybe Domain.FullFarePolicyProgressiveDetails),
    findAllSlabDetailsSlabs :: m [BeamFPSS.FullFarePolicySlabsDetailsSlab],
    findRentalDetails :: m (Maybe Domain.FullFarePolicyRentalDetails),
    findInterCityDetails :: m (Maybe Domain.FullFarePolicyInterCityDetails)
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
      findInterCityDetails = QueriesFPICD.findById' (Id id)
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
  case mFarePolicyDetails of
    Just farePolicyDetails -> do
      return $
        Just
          Domain.FarePolicy
            { id = Id id,
              serviceCharge = mkAmountWithDefault serviceChargeAmount <$> serviceCharge,
              parkingCharge = parkingCharge,
              tollCharges = tollCharges,
              currency = fromMaybe INR currency,
              nightShiftBounds = DPM.NightShiftBounds <$> nightShiftStart <*> nightShiftEnd,
              allowedTripDistanceBounds =
                ((,) <$> minAllowedTripDistance <*> maxAllowedTripDistance) <&> \(minAllowedTripDistance', maxAllowedTripDistance') ->
                  DPM.AllowedTripDistanceBounds
                    { minAllowedTripDistance = minAllowedTripDistance',
                      maxAllowedTripDistance = maxAllowedTripDistance'
                    },
              govtCharges = govtCharges,
              driverExtraFeeBounds = nonEmpty fDEFB,
              farePolicyDetails,
              perMinuteRideExtraTimeCharge = perMinuteRideExtraTimeCharge,
              congestionChargeMultiplier = congestionCharge,
              description = description,
              createdAt = createdAt,
              updatedAt = updatedAt
            }
    Nothing -> return Nothing
