{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FareParameters.FareParametersSlabDetails where

import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FareParameters.FareParametersSlabDetails as BeamFPSD

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FullFareParametersSlabDetails -> m ()
create = createWithKV

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe BeamFPSD.FullFareParametersSlabDetails)
findById' (KTI.Id fareParametersId') = findOneWithKV [Se.Is fareParametersId $ Se.Eq fareParametersId']

instance ToTType' FareParametersSlabDetails BeamFPSD.FullFareParametersSlabDetails where
  toTType' :: BeamFPSD.FullFareParametersSlabDetails -> FareParametersSlabDetails
  toTType' (KTI.Id fareParametersId, Domain.FParamsSlabDetails {..}) = do
    FareParametersSlabDetailsT
      { fareParametersId = fareParametersId,
        platformFee = platformFee,
        sgst = sgst,
        cgst = cgst
      }

instance FromTType' FareParametersSlabDetails BeamFPSD.FullFareParametersSlabDetails where
  fromTType' FareParametersSlabDetailsT {..} = do
    pure $
      Just
        ( KTI.Id fareParametersId,
          Domain.FParamsSlabDetails
            { platformFee = platformFee,
              sgst = sgst,
              cgst = cgst
            }
        )
