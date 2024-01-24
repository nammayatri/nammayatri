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
  ( FromTType' (fromTType'),
    ToTType' (toTType'),
    createWithKV,
    findOneWithKV,
    updateOneWithKV,
  )
import Kernel.Prelude hiding (toList)
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy as BeamFP
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails as BeamFPRD
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QueriesDEFB
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
      Se.Set BeamFP.perMinuteRideExtraTimeCharge $ farePolicy.perMinuteRideExtraTimeCharge,
      Se.Set BeamFP.description $ farePolicy.description,
      Se.Set BeamFP.updatedAt now
    ]
    [Se.Is BeamFP.id (Se.Eq $ getId farePolicy.id)]

  case farePolicy.farePolicyDetails of
    ProgressiveDetails fPPD ->
      updateOneWithKV
        [ Se.Set BeamFPPD.baseFare $ fPPD.baseFare,
          Se.Set BeamFPPD.baseDistance $ fPPD.baseDistance,
          Se.Set BeamFPPD.deadKmFare $ fPPD.deadKmFare,
          Se.Set BeamFPPD.waitingCharge $ (.waitingCharge) <$> fPPD.waitingChargeInfo,
          Se.Set BeamFPPD.freeWatingTime $ (.freeWaitingTime) <$> fPPD.waitingChargeInfo,
          Se.Set BeamFPPD.nightShiftCharge $ fPPD.nightShiftCharge
        ]
        [Se.Is BeamFPPD.farePolicyId (Se.Eq $ getId farePolicy.id)]
    SlabsDetails (FPSlabsDetails _slabs) -> pure ()
    RentalDetails _ -> pure ()

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FarePolicy -> m ()
update farePolicy = do
  now <- getCurrentTime
  void $
    updateOneWithKV
      [ Se.Set BeamFP.nightShiftStart $ (.nightShiftStart) <$> farePolicy.nightShiftBounds,
        Se.Set BeamFP.nightShiftEnd $ (.nightShiftEnd) <$> farePolicy.nightShiftBounds,
        Se.Set BeamFP.updatedAt now
      ]
      [Se.Is BeamFP.id (Se.Eq $ getId farePolicy.id)]

  QueriesDEFB.deleteAll' farePolicy.id
  case farePolicy.driverExtraFeeBounds of
    Just driverExtraFeeBounds -> mapM_ (\defb -> QueriesDEFB.create (farePolicy.id, defb)) (toList driverExtraFeeBounds)
    Nothing -> pure ()

  case farePolicy.farePolicyDetails of
    ProgressiveDetails fPPD ->
      updateOneWithKV
        [ Se.Set BeamFPPD.baseFare $ fPPD.baseFare,
          Se.Set BeamFPPD.baseDistance $ fPPD.baseDistance,
          Se.Set BeamFPPD.deadKmFare $ fPPD.deadKmFare,
          Se.Set BeamFPPD.nightShiftCharge $ fPPD.nightShiftCharge
        ]
        [Se.Is BeamFPPD.farePolicyId (Se.Eq $ getId farePolicy.id)]
    SlabsDetails (FPSlabsDetails slabs) -> do
      _ <- QueriesFPSDS.deleteAll' farePolicy.id
      mapM_ (create'' farePolicy.id) slabs
    RentalDetails fPRD ->
      updateOneWithKV
        [ Se.Set BeamFPRD.baseFare $ fPRD.baseFare,
          Se.Set BeamFPRD.perHourCharge $ fPRD.perHourCharge,
          Se.Set BeamFPRD.perHourFreeKms $ fPRD.perHourFreeKms,
          Se.Set BeamFPRD.perExtraKmRate $ fPRD.perExtraKmRate,
          Se.Set BeamFPRD.nightShiftCharge $ fPRD.nightShiftCharge
        ]
        [Se.Is BeamFPRD.farePolicyId (Se.Eq $ getId farePolicy.id)]
  where
    create'' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id FarePolicy -> FPSlabsDetailsSlab -> m ()
    create'' id' slab = createWithKV (id', slab)

instance ToTType' BeamFP.FarePolicy FarePolicy where
  toTType' FarePolicy {..} = do
    BeamFP.FarePolicyT
      { BeamFP.id = getId id,
        BeamFP.serviceCharge = serviceCharge,
        BeamFP.nightShiftStart = (.nightShiftStart) <$> nightShiftBounds,
        BeamFP.nightShiftEnd = (.nightShiftEnd) <$> nightShiftBounds,
        BeamFP.maxAllowedTripDistance = (.maxAllowedTripDistance) <$> allowedTripDistanceBounds,
        BeamFP.minAllowedTripDistance = (.minAllowedTripDistance) <$> allowedTripDistanceBounds,
        BeamFP.govtCharges = govtCharges,
        BeamFP.perMinuteRideExtraTimeCharge = perMinuteRideExtraTimeCharge,
        BeamFP.farePolicyType = getFarePolicyType $ FarePolicy {..},
        BeamFP.description = description,
        BeamFP.createdAt = createdAt,
        BeamFP.updatedAt = updatedAt
      }

instance FromTType' BeamFP.FarePolicy Domain.FarePolicy where
  fromTType' BeamFP.FarePolicyT {..} = do
    fullDEFB <- QueriesDEFB.findAll' (KTI.Id id)
    let fDEFB = snd <$> fullDEFB
    mFarePolicyDetails <-
      case farePolicyType of
        Progressive -> do
          mFPPD <- QueriesFPPD.findById' (Id id)
          case mFPPD of
            Just (_, fPPD) -> return $ Just (ProgressiveDetails fPPD)
            Nothing -> return Nothing
        Slabs -> do
          fullSlabs <- QueriesFPSDS.findAll' (Id id)
          let slabs = snd <$> fullSlabs
          case nonEmpty slabs of
            Just nESlabs -> return $ Just (SlabsDetails (FPSlabsDetails nESlabs))
            Nothing -> return Nothing
        Rental -> do
          mFPRD <- QueriesFPRD.findById' (Id id)
          case mFPRD of
            Just (_, fPRD) -> return $ Just (RentalDetails fPRD)
            Nothing -> return Nothing
    case mFarePolicyDetails of
      Just farePolicyDetails -> do
        return $
          Just
            Domain.FarePolicy
              { id = Id id,
                serviceCharge = serviceCharge,
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
                description = description,
                createdAt = createdAt,
                updatedAt = updatedAt
              }
      Nothing -> return Nothing
