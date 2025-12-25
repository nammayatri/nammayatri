{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection where

import Data.Text as Text
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection as Domain
import Kernel.Beam.Functions as KBF
import Kernel.Prelude as KP
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Storage.Beam.FullFarePolicyProgressiveDetailsPerMinRateSection as BeamFPPDPM
import Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection ()
import Utils.Common.CacUtils

findFarePolicyProgressiveDetailsPerMinRateSectionFromCAC :: (CacheFlow m r, EsqDBFlow m r) => [(CacContext, Value)] -> String -> Id DFP.FarePolicy -> Int -> m [Domain.FullFarePolicyProgressiveDetailsPerMinRateSection]
findFarePolicyProgressiveDetailsPerMinRateSectionFromCAC context tenant id toss = do
  mbRes :: (Maybe [BeamFPPDPM.FullFarePolicyProgressiveDetailsPerMinRateSection]) <- getConfigListFromCac context tenant toss FullFarePolicyProgressiveDetailsPerMinRateSection (Text.unpack id.getId)
  let configs = KP.mapM fromCacType (fromMaybe [] mbRes)
  catMaybes <$> configs

instance FromCacType BeamFPPDPM.FullFarePolicyProgressiveDetailsPerMinRateSection Domain.FullFarePolicyProgressiveDetailsPerMinRateSection where
  fromCacType = fromTType'
