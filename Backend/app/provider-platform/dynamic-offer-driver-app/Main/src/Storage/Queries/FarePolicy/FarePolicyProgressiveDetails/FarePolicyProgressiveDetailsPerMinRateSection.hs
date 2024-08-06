{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Storage.Queries.FullFarePolicyProgressiveDetailsPerMinRateSection as QueriesFPPDPM

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => (Text, Domain.FPProgressiveDetailsPerMinRateSection) -> m ()
create = createWithKV . Domain.makeFullFPPDPerMinRateSection

findAll ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DFP.FarePolicy ->
  m [Domain.FPProgressiveDetailsPerMinRateSection]
findAll (Id farePolicyId') = do
  fullFP <- QueriesFPPDPM.findAllByFarePolicyId farePolicyId'
  return $ Domain.makeFPProgressiveDetailsPerMinRateSection <$> fullFP

deleteAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
deleteAll' = QueriesFPPDPM.deleteAllByFarePolicyId
