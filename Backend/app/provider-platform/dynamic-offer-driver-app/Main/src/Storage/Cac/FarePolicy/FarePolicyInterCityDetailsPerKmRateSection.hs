{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.FarePolicy.FarePolicyInterCityDetailsPerKmRateSection where

import qualified Data.Text as Text
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection as Domain
import Kernel.Beam.Functions as KBF
import Kernel.Prelude as KP
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Storage.Beam.FullFarePolicyInterCityDetailsPerKmRateSection as BeamFPICDPerKm
import Storage.Queries.FullFarePolicyInterCityDetailsPerKmRateSection ()
import Utils.Common.CacUtils

findFarePolicyInterCityDetailsPerKmRateSectionFromCAC :: (CacheFlow m r, EsqDBFlow m r) => [(CacContext, Value)] -> String -> Id DFP.FarePolicy -> Int -> m [Domain.FullFarePolicyInterCityDetailsPerKmRateSection]
findFarePolicyInterCityDetailsPerKmRateSectionFromCAC context tenant id toss = do
  mbRes :: (Maybe [BeamFPICDPerKm.FullFarePolicyInterCityDetailsPerKmRateSection]) <- getConfigListFromCac context tenant toss FullFarePolicyInterCityDetailsPerKmRateSection (Text.unpack id.getId)
  let configs = KP.mapM fromCacType (fromMaybe [] mbRes)
  catMaybes <$> configs

instance FromCacType BeamFPICDPerKm.FullFarePolicyInterCityDetailsPerKmRateSection Domain.FullFarePolicyInterCityDetailsPerKmRateSection where
  fromCacType = fromTType'
