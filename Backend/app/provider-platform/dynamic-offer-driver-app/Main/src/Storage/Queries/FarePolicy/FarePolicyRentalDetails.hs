{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyRentalDetails where

import Data.List.NonEmpty (nonEmpty)
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyRentalDetails as BeamFPRD
import qualified Storage.Queries.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as QueriesFPRDB

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe Domain.FullFarePolicyRentalDetails)
findById' (KTI.Id farePolicyId') = findOneWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq farePolicyId']

instance FromTType' BeamFPRD.FarePolicyRentalDetails Domain.FullFarePolicyRentalDetails where
  fromTType' BeamFPRD.FarePolicyRentalDetailsT {..} = do
    fullFPRDB <- QueriesFPRDB.findAll' (KTI.Id farePolicyId)
    fPRDB <- fromMaybeM (InternalError "No distance buffer found for rental") (nonEmpty fullFPRDB)
    pure $
      Just
        ( KTI.Id farePolicyId,
          Domain.FPRentalDetails
            { baseFare = baseFare,
              perHourCharge = perHourCharge,
              perHourFreeKms = perHourFreeKms,
              perExtraKmRate = perExtraKmRate,
              nightShiftCharge = nightShiftCharge,
              distanceBuffers = snd <$> fPRDB
            }
        )

instance ToTType' BeamFPRD.FarePolicyRentalDetails Domain.FullFarePolicyRentalDetails where
  toTType' (KTI.Id farePolicyId, Domain.FPRentalDetails {..}) =
    BeamFPRD.FarePolicyRentalDetailsT
      { farePolicyId = farePolicyId,
        baseFare = baseFare,
        perHourCharge = perHourCharge,
        perHourFreeKms = perHourFreeKms,
        perExtraKmRate = perExtraKmRate,
        nightShiftCharge = nightShiftCharge
      }
