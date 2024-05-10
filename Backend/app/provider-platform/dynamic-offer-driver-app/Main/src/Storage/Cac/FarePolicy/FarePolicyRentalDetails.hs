{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.FarePolicy.FarePolicyRentalDetails where

import Data.List.NonEmpty
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Storage.Beam.FarePolicy.FarePolicyRentalDetails as BeamFPRD
import qualified Storage.Cac.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as CQFPRDB
import Utils.Common.CacUtils

findFarePolicyRentalDetailsFromCAC :: (CacheFlow m r, EsqDBFlow m r) => [(CacContext, Value)] -> String -> Id Domain.FarePolicy -> Int -> m (Maybe Domain.FullFarePolicyRentalDetails)
findFarePolicyRentalDetailsFromCAC context tenant id toss = do
  res :: (Maybe BeamFPRD.FarePolicyRentalDetails) <- getConfigFromCac context tenant toss FarePolicyRentalDetails
  case res of
    Nothing -> pure Nothing
    Just config -> fromCacType (config, context, tenant, id, toss)

instance FromCacType (BeamFPRD.FarePolicyRentalDetails, [(CacContext, Value)], String, Id Domain.FarePolicy, Int) Domain.FullFarePolicyRentalDetails where
  fromCacType (BeamFPRD.FarePolicyRentalDetailsT {..}, context, tenant, id, toss) = do
    fullFPRDB <- CQFPRDB.findFarePolicyRentalDetailsDistanceBuffersFromCAC context tenant id toss
    case nonEmpty fullFPRDB of
      Nothing -> pure Nothing
      Just fPRDB ->
        pure $
          Just
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
