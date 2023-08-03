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

import Data.List.NonEmpty
import Domain.Types.FarePolicy as Domain
import qualified EulerHS.Language as L
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
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as QueriesDEFB
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails as QueriesFPPD
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as QueriesFPSDS

findById :: (L.MonadFlow m, Log m) => Id FarePolicy -> m (Maybe FarePolicy)
findById (Id farePolicyId) = findOneWithKV [Se.Is BeamFP.id $ Se.Eq farePolicyId]

update :: (L.MonadFlow m, MonadTime m, Log m) => FarePolicy -> m ()
update farePolicy = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamFP.nightShiftStart $ Domain.nightShiftStart <$> farePolicy.nightShiftBounds,
      Se.Set BeamFP.nightShiftEnd $ Domain.nightShiftStart <$> farePolicy.nightShiftBounds,
      Se.Set BeamFP.updatedAt now
    ]
    [Se.Is BeamFP.id (Se.Eq $ getId farePolicy.id)]
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
  where
    create'' :: (L.MonadFlow m, Log m) => Id FarePolicy -> FPSlabsDetailsSlab -> m ()
    create'' id' slab = createWithKV (id', slab)

instance ToTType' BeamFP.FarePolicy FarePolicy where
  toTType' FarePolicy {..} = do
    BeamFP.FarePolicyT
      { BeamFP.id = getId id,
        BeamFP.serviceCharge = serviceCharge,
        BeamFP.nightShiftStart = Domain.nightShiftStart <$> nightShiftBounds,
        BeamFP.nightShiftEnd = Domain.nightShiftEnd <$> nightShiftBounds,
        BeamFP.maxAllowedTripDistance = Domain.maxAllowedTripDistance <$> allowedTripDistanceBounds,
        BeamFP.minAllowedTripDistance = Domain.minAllowedTripDistance <$> allowedTripDistanceBounds,
        BeamFP.govtCharges = govtCharges,
        BeamFP.farePolicyType = getFarePolicyType $ FarePolicy {..},
        BeamFP.description = description,
        BeamFP.createdAt = createdAt,
        BeamFP.updatedAt = updatedAt
      }

instance FromTType' BeamFP.FarePolicy Domain.FarePolicy where
  fromTType' BeamFP.FarePolicyT {..} = do
    fullDEFB <- QueriesDEFB.findAll' (KTI.Id id)
    let fDEFB = snd <$> fullDEFB
    fullFPPD <- QueriesFPPD.findById' (Id id)
    if isJust fullFPPD
      then do
        fullslabs <- QueriesFPSDS.findAll' (Id id)
        let fPPD = snd $ fromJust fullFPPD
        let slabs = snd <$> fullslabs
        pure $
          Just
            Domain.FarePolicy
              { id = Id id,
                serviceCharge = serviceCharge,
                nightShiftBounds = NightShiftBounds <$> nightShiftStart <*> nightShiftEnd,
                allowedTripDistanceBounds =
                  ((,) <$> minAllowedTripDistance <*> maxAllowedTripDistance) <&> \(minAllowedTripDistance', maxAllowedTripDistance') ->
                    Domain.AllowedTripDistanceBounds
                      { minAllowedTripDistance = minAllowedTripDistance',
                        maxAllowedTripDistance = maxAllowedTripDistance'
                      },
                govtCharges = govtCharges,
                driverExtraFeeBounds = nonEmpty fDEFB,
                farePolicyDetails = case farePolicyType of
                  Progressive -> ProgressiveDetails fPPD
                  Slabs -> SlabsDetails (FPSlabsDetails (fromJust $ nonEmpty slabs)),
                description = description,
                createdAt = createdAt,
                updatedAt = updatedAt
              }
      else pure Nothing
