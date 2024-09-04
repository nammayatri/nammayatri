{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs where

import qualified Domain.Types.FarePolicy as DFP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs as BeamFPRDPS

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => BeamFPRDPS.FullFarePolicyRentalDetailsPricingSlabs -> m ()
create = createWithKV

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id DFP.FarePolicy -> m (Maybe BeamFPRDPS.FullFarePolicyRentalDetailsPricingSlabs)
findById' farePolicyId' = findOneWithKV [Se.Is BeamFPRDPS.farePolicyId $ Se.Eq (getId farePolicyId')]

findAll' ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DFP.FarePolicy ->
  m [BeamFPRDPS.FullFarePolicyRentalDetailsPricingSlabs]
findAll' farePolicyId = findAllWithOptionsKV [Se.Is BeamFPRDPS.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamFPRDPS.distancePercentage) Nothing Nothing

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
delete farePolicyId = deleteWithKV [Se.Is BeamFPRDPS.farePolicyId $ Se.Eq (getId farePolicyId)]

instance FromTType' BeamFPRDPS.FarePolicyRentalDetailsPricingSlabs BeamFPRDPS.FullFarePolicyRentalDetailsPricingSlabs where
  fromTType' BeamFPRDPS.FarePolicyRentalDetailsPricingSlabsT {..} = do
    pure $
      Just
        ( KTI.Id farePolicyId,
          DFP.FPRentalDetailsPricingSlabs {..}
        )

instance ToTType' BeamFPRDPS.FarePolicyRentalDetailsPricingSlabs BeamFPRDPS.FullFarePolicyRentalDetailsPricingSlabs where
  toTType' (KTI.Id farePolicyId, DFP.FPRentalDetailsPricingSlabs {..}) =
    BeamFPRDPS.FarePolicyRentalDetailsPricingSlabsT {..}
