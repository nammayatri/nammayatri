{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers where

import qualified Domain.Types.FarePolicy as DFP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as BeamFPRDDB

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id DFP.FarePolicy -> m (Maybe BeamFPRDDB.FullFarePolicyRentalDetailsDistanceBuffers)
findById' farePolicyId' = findOneWithKV [Se.Is BeamFPRDDB.farePolicyId $ Se.Eq (getId farePolicyId')]

findAll' ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DFP.FarePolicy ->
  m [BeamFPRDDB.FullFarePolicyRentalDetailsDistanceBuffers]
findAll' farePolicyId = findAllWithOptionsKV [Se.Is BeamFPRDDB.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamFPRDDB.rideDuration) Nothing Nothing

instance FromTType' BeamFPRDDB.FarePolicyRentalDetailsDistanceBuffers BeamFPRDDB.FullFarePolicyRentalDetailsDistanceBuffers where
  fromTType' BeamFPRDDB.FarePolicyRentalDetailsDistanceBuffersT {..} = do
    pure $
      Just
        ( KTI.Id farePolicyId,
          DFP.FPRentalDetailsDistanceBuffers
            { rideDuration = rideDuration,
              bufferKms = bufferKms
            }
        )

instance ToTType' BeamFPRDDB.FarePolicyRentalDetailsDistanceBuffers BeamFPRDDB.FullFarePolicyRentalDetailsDistanceBuffers where
  toTType' (KTI.Id farePolicyId, DFP.FPRentalDetailsDistanceBuffers {..}) =
    BeamFPRDDB.FarePolicyRentalDetailsDistanceBuffersT
      { farePolicyId = farePolicyId,
        rideDuration = rideDuration,
        bufferKms = bufferKms
      }
