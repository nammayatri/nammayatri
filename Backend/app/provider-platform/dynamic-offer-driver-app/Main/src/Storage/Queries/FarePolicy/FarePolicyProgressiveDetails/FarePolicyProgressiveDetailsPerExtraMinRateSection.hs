{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraMinRateSection where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraMinRateSection as Domain
import qualified Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection as Domain
import Kernel.Prelude
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Storage.Queries.FullFarePolicyProgressiveDetailsPerExtraMinRateSection as BeamFPPM

findAll ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DFP.FarePolicy ->
  m [Domain.FPProgressiveDetailsPerExtraMinRateSection]
findAll farePolicyId' = do
  fullFP <- BeamFPPM.findAllByFarePolicyId Nothing Nothing farePolicyId'
  return $ (\Domain.FullFarePolicyProgressiveDetailsPerExtraMinRateSection {..} -> Domain.FPProgressiveDetailsPerExtraMinRateSection {..}) <$> fullFP
