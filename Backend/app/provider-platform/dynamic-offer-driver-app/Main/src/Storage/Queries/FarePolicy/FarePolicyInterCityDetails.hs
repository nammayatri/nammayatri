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

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyInterCityDetails as BeamFPRD
import qualified Storage.Beam.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as BeamFPICDPS
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as BeamFPPDP
import qualified Storage.Queries.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as QueriesFPICDPS
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QueriesFPPDP

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe Domain.FullFarePolicyInterCityDetails)
findById' (KTI.Id farePolicyId') = findOneWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq farePolicyId']

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.FullFarePolicyInterCityDetails -> m ()
create farePolicyInterCityDetails = do
  mapM_ QueriesFPICDPS.create (map (fst farePolicyInterCityDetails,) (NE.toList (snd farePolicyInterCityDetails).pricingSlabs))
  mapM_ QueriesFPPDP.create (map (fst farePolicyInterCityDetails,) (NE.toList (snd farePolicyInterCityDetails).perKmRateSections))
  createWithKV farePolicyInterCityDetails

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = do
  QueriesFPICDPS.delete farePolicyId
  deleteWithKV [Se.Is BeamFPPDP.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]
  deleteWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]

instance FromTType' BeamFPRD.FarePolicyInterCityDetails Domain.FullFarePolicyInterCityDetails where
  fromTType' farePolicyInterCityDetails = do
    fullFPPDP <- QueriesFPPDP.findAll' (KTI.Id farePolicyInterCityDetails.farePolicyId)
    fullFPICDPS <- QueriesFPICDPS.findAll' (KTI.Id farePolicyInterCityDetails.farePolicyId)
    fPICDPS <- fromMaybeM (InternalError "No pricing slab found for intercity") (NE.nonEmpty fullFPICDPS) -- check it
    fPPDP <- fromMaybeM (InternalError "No progressive rates found for intercity") (NE.nonEmpty fullFPPDP)
    pure . Just $ fromTTypeFarePolicyInterCityDetails farePolicyInterCityDetails fPICDPS fPPDP

fromTTypeFarePolicyInterCityDetails ::
  BeamFPRD.FarePolicyInterCityDetails ->
  NonEmpty BeamFPICDPS.FullFarePolicyInterCityDetailsPricingSlabs ->
  NonEmpty BeamFPPDP.FullFarePolicyProgressiveDetailsPerExtraKmRateSection ->
  Domain.FullFarePolicyInterCityDetails
fromTTypeFarePolicyInterCityDetails BeamFPRD.FarePolicyInterCityDetailsT {..} fPICDPS fullFPPDP =
  ( KTI.Id farePolicyId,
    Domain.FPInterCityDetails {pricingSlabs = snd <$> fPICDPS, perKmRateSections = snd <$> fullFPPDP, ..}
  )

instance ToTType' BeamFPRD.FarePolicyInterCityDetails Domain.FullFarePolicyInterCityDetails where
  toTType' (KTI.Id farePolicyId, Domain.FPInterCityDetails {..}) =
    BeamFPRD.FarePolicyInterCityDetailsT
      { farePolicyId = farePolicyId,
        ..
      }
