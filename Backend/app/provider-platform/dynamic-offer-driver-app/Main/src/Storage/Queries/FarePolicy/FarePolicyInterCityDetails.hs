{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyInterCityDetails where

import qualified Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyInterCityDetails as BeamFPRD

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe Domain.FullFarePolicyInterCityDetails)
findById' (KTI.Id farePolicyId') = findOneWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq farePolicyId']

instance FromTType' BeamFPRD.FarePolicyInterCityDetails Domain.FullFarePolicyInterCityDetails where
  fromTType' farePolicyInterCityDetails = do
    pure . Just $ fromTTypeFarePolicyInterCityDetails farePolicyInterCityDetails

fromTTypeFarePolicyInterCityDetails ::
  BeamFPRD.FarePolicyInterCityDetails ->
  Domain.FullFarePolicyInterCityDetails
fromTTypeFarePolicyInterCityDetails BeamFPRD.FarePolicyInterCityDetailsT {..} =
  ( KTI.Id farePolicyId,
    Domain.FPInterCityDetails {..}
  )

instance ToTType' BeamFPRD.FarePolicyInterCityDetails Domain.FullFarePolicyInterCityDetails where
  toTType' (KTI.Id farePolicyId, Domain.FPInterCityDetails {..}) =
    BeamFPRD.FarePolicyInterCityDetailsT
      { farePolicyId = farePolicyId,
        ..
      }
