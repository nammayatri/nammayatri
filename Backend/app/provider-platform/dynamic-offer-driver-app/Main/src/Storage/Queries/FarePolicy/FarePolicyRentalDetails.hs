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

import Data.List.NonEmpty (nonEmpty)
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyRentalDetails as BeamFPRD
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as BeamFPRDDB
import qualified Storage.Queries.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as QueriesFPRDB

findById' :: KvDbFlow m r => KTI.Id Domain.FarePolicy -> m (Maybe Domain.FullFarePolicyRentalDetails)
findById' (KTI.Id farePolicyId') = findOneWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq farePolicyId']

instance FromTType' BeamFPRD.FarePolicyRentalDetails Domain.FullFarePolicyRentalDetails where
  fromTType' farePolicyRentalDetails = do
    fullFPRDB <- QueriesFPRDB.findAll' (KTI.Id farePolicyRentalDetails.farePolicyId)
    fPRDB <- fromMaybeM (InternalError "No distance buffer found for rental") (nonEmpty fullFPRDB)
    pure . Just $ fromTTypeFarePolicyRentalDetails farePolicyRentalDetails fPRDB

fromTTypeFarePolicyRentalDetails ::
  BeamFPRD.FarePolicyRentalDetails ->
  NonEmpty BeamFPRDDB.FullFarePolicyRentalDetailsDistanceBuffers ->
  Domain.FullFarePolicyRentalDetails
fromTTypeFarePolicyRentalDetails BeamFPRD.FarePolicyRentalDetailsT {..} fPRDB =
  ( KTI.Id farePolicyId,
    Domain.FPRentalDetails
      { baseFare = mkAmountWithDefault baseFareAmount baseFare,
        perHourCharge = mkAmountWithDefault perHourChargeAmount perHourCharge,
        perExtraMinRate = mkAmountWithDefault perExtraMinRateAmount perExtraMinRate,
        perExtraKmRate = mkAmountWithDefault perExtraKmRateAmount perExtraKmRate,
        nightShiftCharge = nightShiftCharge,
        includedKmPerHr = includedKmPerHr,
        plannedPerKmRate = mkAmountWithDefault plannedPerKmRateAmount plannedPerKmRate,
        maxAdditionalKmsLimit = maxAdditionalKmsLimit,
        totalAdditionalKmsLimit = totalAdditionalKmsLimit,
        distanceBuffers = snd <$> fPRDB,
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
        nightShiftCharge = nightShiftCharge,
        includedKmPerHr = includedKmPerHr,
        maxAdditionalKmsLimit = maxAdditionalKmsLimit,
        totalAdditionalKmsLimit = totalAdditionalKmsLimit,
        plannedPerKmRate = roundToIntegral plannedPerKmRate,
        plannedPerKmRateAmount = Just plannedPerKmRate,
        currency = Just currency
      }
