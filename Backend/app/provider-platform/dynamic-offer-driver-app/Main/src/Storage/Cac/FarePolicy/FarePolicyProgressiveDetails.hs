{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.FarePolicy.FarePolicyProgressiveDetails where

import Data.List.NonEmpty
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection as Domain
import Kernel.Beam.Functions
import Kernel.Prelude as KP
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Cac.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as CQueriesFPPDP
import qualified Storage.Cac.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection as CQueriesFPPDPM
import Storage.Queries.FarePolicy.FarePolicyProgressiveDetails (fromTTypeFarePolicyProgressiveDetails)
import Utils.Common.CacUtils

getFPProgressiveDetailsFromCAC :: (CacheFlow m r, EsqDBFlow m r) => [(CacContext, Value)] -> String -> Id DFP.FarePolicy -> Int -> m (Maybe DFP.FullFarePolicyProgressiveDetails)
getFPProgressiveDetailsFromCAC context tenant id toss = do
  res :: (Maybe BeamFPPD.FarePolicyProgressiveDetails) <- getConfigFromCac context tenant toss FarePolicyProgressiveDetails
  case res of
    Nothing -> pure Nothing
    Just fPPD -> fromCacType (fPPD, context, tenant, id, toss)

instance FromCacType (BeamFPPD.FarePolicyProgressiveDetails, [(CacContext, Value)], String, Id DFP.FarePolicy, Int) DFP.FullFarePolicyProgressiveDetails where
  fromCacType (farePolicyProgressiveDetails, context, tenant, id, toss) = do
    fullFPPDP <- CQueriesFPPDP.findFarePolicyProgressiveDetailsPerExtraKmRateSectionFromCAC context tenant id toss
    fullFPPDPM <- KP.map Domain.makeFPProgressiveDetailsPerMinRateSection <$> CQueriesFPPDPM.findFarePolicyProgressiveDetailsPerMinRateSectionFromCAC context tenant id toss
    pure $ fromTTypeFarePolicyProgressiveDetails farePolicyProgressiveDetails (nonEmpty fullFPPDPM) <$> nonEmpty fullFPPDP
