{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.FarePolicy.FarePolicyInterCityDetails where

import Data.List.NonEmpty hiding (map)
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.FarePolicy.FarePolicyInterCityDetails as BeamFPRD
import qualified Storage.Cac.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as CQFPTCDPS
import qualified Storage.Cac.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as CQueriesFPPDP
import Storage.Queries.FarePolicy.FarePolicyInterCityDetails (fromTTypeFarePolicyInterCityDetails)
import Utils.Common.CacUtils

findFarePolicyInterCityDetailsFromCAC :: (CacheFlow m r, EsqDBFlow m r) => [(CacContext, Value)] -> String -> Id Domain.FarePolicy -> Int -> m (Maybe Domain.FullFarePolicyInterCityDetails)
findFarePolicyInterCityDetailsFromCAC context tenant id toss = do
  res :: (Maybe BeamFPRD.FarePolicyInterCityDetails) <- getConfigFromCac context tenant toss FarePolicyInterCityDetails
  case res of
    Nothing -> pure Nothing
    Just config -> fromCacType (config, context, tenant, id, toss)

instance FromCacType (BeamFPRD.FarePolicyInterCityDetails, [(CacContext, Value)], String, Id Domain.FarePolicy, Int) Domain.FullFarePolicyInterCityDetails where
  fromCacType (farePolicyInterCityDetails, context, tenant, id, toss) = do
    fullFPICDPS <- CQFPTCDPS.findFarePolicyInterCityDetailsPricingSlabsFromCAC context tenant id toss
    fullFPPDP <- CQueriesFPPDP.findFarePolicyProgressiveDetailsPerExtraKmRateSectionFromCAC context tenant id toss
    case (nonEmpty fullFPICDPS, nonEmpty fullFPPDP) of
      (Just fPICDPS, Just fPPDP) -> pure $ Just $ fromTTypeFarePolicyInterCityDetails farePolicyInterCityDetails fPICDPS fPPDP
      _ -> pure Nothing
