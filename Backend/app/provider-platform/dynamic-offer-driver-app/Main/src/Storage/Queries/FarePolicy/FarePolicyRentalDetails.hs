{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyRentalDetails where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyRentalDetails as BeamFPRD
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as BeamFPRDDB
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs as BeamFPRDPS
import qualified Storage.Queries.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as QueriesFPRDB
import qualified Storage.Queries.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs as QueriesFPRDPS

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe Domain.FullFarePolicyRentalDetails)
findById' (KTI.Id farePolicyId') = findOneWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq farePolicyId']

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.FullFarePolicyRentalDetails -> m ()
create farePolicyRentalDetails = do
  mapM_ QueriesFPRDB.create (map (fst farePolicyRentalDetails,) (NE.toList (snd farePolicyRentalDetails).distanceBuffers))
  mapM_ QueriesFPRDPS.create (map (fst farePolicyRentalDetails,) (NE.toList (snd farePolicyRentalDetails).pricingSlabs))
  createWithKV farePolicyRentalDetails

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = do
  QueriesFPRDB.delete farePolicyId
  QueriesFPRDPS.delete farePolicyId
  deleteWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]

instance FromTType' BeamFPRD.FarePolicyRentalDetails Domain.FullFarePolicyRentalDetails where
  fromTType' farePolicyRentalDetails = do
    fullFPRDB <- QueriesFPRDB.findAll' (KTI.Id farePolicyRentalDetails.farePolicyId)
    fPRDB <- fromMaybeM (InternalError "No distance buffer found for rental") (NE.nonEmpty fullFPRDB)
    fullFPRDPS <- QueriesFPRDPS.findAll' (KTI.Id farePolicyRentalDetails.farePolicyId)
    fPRDPS <- fromMaybeM (InternalError "No pricing slab found for rental") (NE.nonEmpty fullFPRDPS) -- check it
    pure . Just $ fromTTypeFarePolicyRentalDetails farePolicyRentalDetails fPRDB fPRDPS

fromTTypeFarePolicyRentalDetails ::
  BeamFPRD.FarePolicyRentalDetails ->
  NonEmpty BeamFPRDDB.FullFarePolicyRentalDetailsDistanceBuffers ->
  NonEmpty BeamFPRDPS.FullFarePolicyRentalDetailsPricingSlabs ->
  Domain.FullFarePolicyRentalDetails
fromTTypeFarePolicyRentalDetails BeamFPRD.FarePolicyRentalDetailsT {..} fPRDB fPRDPS =
  ( KTI.Id farePolicyId,
    Domain.FPRentalDetails
      { baseFare = mkAmountWithDefault baseFareAmount baseFare,
        perHourCharge = mkAmountWithDefault perHourChargeAmount perHourCharge,
        perExtraMinRate = mkAmountWithDefault perExtraMinRateAmount perExtraMinRate,
        perExtraKmRate = mkAmountWithDefault perExtraKmRateAmount perExtraKmRate,
        nightShiftCharge = nightShiftCharge,
        includedKmPerHr = includedKmPerHr,
        deadKmFare = deadKmFare,
        plannedPerKmRate = mkAmountWithDefault plannedPerKmRateAmount plannedPerKmRate,
        maxAdditionalKmsLimit = maxAdditionalKmsLimit,
        totalAdditionalKmsLimit = totalAdditionalKmsLimit,
        distanceBuffers = snd <$> fPRDB,
        pricingSlabs = snd <$> fPRDPS,
        waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWaitingTime) <&> \(waitingCharge', freeWaitingTime') ->
            Domain.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              },
        currency = fromMaybe INR currency
      }
  )

instance ToTType' BeamFPRD.FarePolicyRentalDetails Domain.FullFarePolicyRentalDetails where
  toTType' (KTI.Id farePolicyId, Domain.FPRentalDetails {..}) =
    BeamFPRD.FarePolicyRentalDetailsT
      { farePolicyId = farePolicyId,
        baseFare = roundToIntegral baseFare,
        perHourCharge = roundToIntegral perHourCharge,
        perExtraMinRate = roundToIntegral perExtraMinRate,
        perExtraKmRate = roundToIntegral perExtraKmRate,
        baseFareAmount = Just baseFare,
        perHourChargeAmount = Just perHourCharge,
        perExtraMinRateAmount = Just perExtraMinRate,
        perExtraKmRateAmount = Just perExtraKmRate,
        deadKmFare = deadKmFare,
        nightShiftCharge = nightShiftCharge,
        includedKmPerHr = includedKmPerHr,
        maxAdditionalKmsLimit = maxAdditionalKmsLimit,
        totalAdditionalKmsLimit = totalAdditionalKmsLimit,
        plannedPerKmRate = roundToIntegral plannedPerKmRate,
        plannedPerKmRateAmount = Just plannedPerKmRate,
        freeWaitingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        waitingCharge = (.waitingCharge) <$> waitingChargeInfo,
        currency = Just currency
      }
