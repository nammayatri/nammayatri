{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FareParameters.FareParametersAmbulanceDetails where

import qualified Domain.Types.FareParameters as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FareParameters.FareParametersAmbulanceDetails as BeamFPAD

create :: (MonadFlow m, EsqDBFlow m r) => Domain.FullFareParametersAmbulanceDetails -> m ()
create = createWithKV

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe Domain.FullFareParametersAmbulanceDetails)
findById' (KTI.Id fareParametersId') = findOneWithKV [Se.Is fareParametersId $ Se.Eq fareParametersId']

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> Domain.FParamsAmbulanceDetails -> m ()
update id Domain.FParamsAmbulanceDetails {..} = do
  updateOneWithKV
    [ Se.Set BeamFPAD.distBasedFare distBasedFare
    ]
    [Se.Is BeamFPAD.fareParametersId (Se.Eq id.getId)]

instance FromTType' BeamFPAD.FareParametersAmbulanceDetails Domain.FullFareParametersAmbulanceDetails where
  fromTType' FareParametersAmbulanceDetailsT {..} = do
    pure $
      Just
        ( KTI.Id fareParametersId,
          Domain.FParamsAmbulanceDetails
            { ..
            }
        )

instance ToTType' FareParametersAmbulanceDetails Domain.FullFareParametersAmbulanceDetails where
  toTType' (KTI.Id fareParametersId, Domain.FParamsAmbulanceDetails {..}) =
    FareParametersAmbulanceDetailsT {..}
