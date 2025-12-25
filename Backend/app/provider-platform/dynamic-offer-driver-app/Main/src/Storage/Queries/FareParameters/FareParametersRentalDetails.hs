{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FareParameters.FareParametersRentalDetails where

import qualified Domain.Types.FareParameters as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FareParameters.FareParametersRentalDetails as BeamFPRD

create :: (MonadFlow m, EsqDBFlow m r) => Domain.FullFareParametersRentalDetails -> m ()
create = createWithKV

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe Domain.FullFareParametersRentalDetails)
findById' (KTI.Id fareParametersId') = findOneWithKV [Se.Is fareParametersId $ Se.Eq fareParametersId']

findDeadKmFareEarnings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [KTI.Id Domain.FareParameters] -> m HighPrecMoney
findDeadKmFareEarnings fareParamIds = do
  deadKmFareEarnings <- findAllWithKV [Se.Is BeamFPRD.fareParametersId $ Se.In $ KTI.getId <$> fareParamIds] <&> ((Domain.deadKmFare :: Domain.FParamsRentalDetails -> HighPrecMoney) . snd <$>)
  pure $ sum deadKmFareEarnings

instance FromTType' BeamFPRD.FareParametersRentalDetails Domain.FullFareParametersRentalDetails where
  fromTType' FareParametersRentalDetailsT {..} = do
    pure $
      Just
        ( KTI.Id fareParametersId,
          Domain.FParamsRentalDetails
            { timeBasedFare = mkAmountWithDefault timeBasedFareAmount timeBasedFare,
              distBasedFare = mkAmountWithDefault distBasedFareAmount distBasedFare,
              deadKmFare = fromMaybe 0 deadKmFare,
              extraDistance = fromMaybe 0 extraDistance,
              extraDuration = fromMaybe 0 extraDuration,
              currency = fromMaybe INR currency,
              distanceUnit = fromMaybe Meter distanceUnit,
              ..
            }
        )

instance ToTType' FareParametersRentalDetails Domain.FullFareParametersRentalDetails where
  toTType' (KTI.Id fareParametersId, fParamsRentalDetails) =
    FareParametersRentalDetailsT
      { fareParametersId = fareParametersId,
        timeBasedFare = roundToIntegral $ Domain.timeBasedFare fParamsRentalDetails,
        distBasedFare = roundToIntegral $ (Domain.distBasedFare :: Domain.FParamsRentalDetails -> HighPrecMoney) fParamsRentalDetails,
        timeBasedFareAmount = Just $ Domain.timeBasedFare fParamsRentalDetails,
        distBasedFareAmount = Just $ (Domain.distBasedFare :: Domain.FParamsRentalDetails -> HighPrecMoney) fParamsRentalDetails,
        currency = Just $ (Domain.currency :: Domain.FParamsRentalDetails -> Currency) fParamsRentalDetails,
        extraDistance = Just $ Domain.extraDistance fParamsRentalDetails,
        extraDuration = Just $ Domain.extraDuration fParamsRentalDetails,
        deadKmFare = Just $ (Domain.deadKmFare :: Domain.FParamsRentalDetails -> HighPrecMoney) fParamsRentalDetails,
        distanceUnit = Just $ Domain.distanceUnit fParamsRentalDetails,
        ..
      }
