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
import Kernel.Utils.Common
import Storage.Beam.FarePolicy.FarePolicyRentalDetails as BeamFPRD
import qualified Storage.Cac.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as CQFPRDB
import qualified Storage.Cac.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs as CQFPRDPS
import Storage.Queries.FarePolicy.FarePolicyRentalDetails (fromTTypeFarePolicyRentalDetails)
import Utils.Common.CacUtils

findFarePolicyRentalDetailsFromCAC :: (CacheFlow m r, EsqDBFlow m r) => [(CacContext, Value)] -> String -> Id Domain.FarePolicy -> Int -> m (Maybe Domain.FullFarePolicyRentalDetails)
findFarePolicyRentalDetailsFromCAC context tenant id toss = do
  res :: (Maybe BeamFPRD.FarePolicyRentalDetails) <- getConfigFromCac context tenant toss FarePolicyRentalDetails
  case res of
    Nothing -> pure Nothing
    Just config -> fromCacType (config, context, tenant, id, toss)

instance FromCacType (BeamFPRD.FarePolicyRentalDetails, [(CacContext, Value)], String, Id Domain.FarePolicy, Int) Domain.FullFarePolicyRentalDetails where
  fromCacType (farePolicyRentalDetails, context, tenant, id, toss) = do
    fullFPRDB <- CQFPRDB.findFarePolicyRentalDetailsDistanceBuffersFromCAC context tenant id toss
    fullFPRDPS <- CQFPRDPS.findFarePolicyRentalDetailsPricingSlabsFromCAC context tenant id toss
    case (nonEmpty fullFPRDB, nonEmpty fullFPRDPS) of
      (Just fPRDB, Just fPRDPS) -> pure $ Just $ fromTTypeFarePolicyRentalDetails farePolicyRentalDetails fPRDB fPRDPS
      _ -> pure Nothing
