{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FareParameters.FareParametersProgressiveDetails where

import qualified Domain.Types.FareParameters as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.FareParameters.FareParametersProgressiveDetails as BeamFPPD

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.FullFareParametersProgressiveDetails -> m ()
create = createWithKV

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FareParameters -> m (Maybe Domain.FullFareParametersProgressiveDetails)
findById' (KTI.Id fareParametersId') = findOneWithKV [Se.Is fareParametersId $ Se.Eq fareParametersId']

findDeadKmFareEarnings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [KTI.Id Domain.FareParameters] -> m HighPrecMoney
findDeadKmFareEarnings fareParamIds = do
  deadKmFareEarnings <- findAllWithKV [Se.Is BeamFPPD.fareParametersId $ Se.In $ KTI.getId <$> fareParamIds] <&> ((Domain.deadKmFare :: Domain.FParamsProgressiveDetails -> HighPrecMoney) . snd <$>)
  pure $ sum deadKmFareEarnings

instance FromTType' BeamFPPD.FareParametersProgressiveDetails Domain.FullFareParametersProgressiveDetails where
  fromTType' FareParametersProgressiveDetailsT {..} = do
    pure $
      Just
        ( KTI.Id fareParametersId,
          Domain.FParamsProgressiveDetails
            { deadKmFare = mkAmountWithDefault deadKmFareAmount deadKmFare,
              extraKmFare = mkAmountWithDefault extraKmFareAmount <$> extraKmFare,
              rideDurationFare = rideDurationFare,
              currency = fromMaybe INR currency
            }
        )

instance ToTType' FareParametersProgressiveDetails Domain.FullFareParametersProgressiveDetails where
  toTType' (KTI.Id fareParametersId, fParamsProgressiveDetails) =
    FareParametersProgressiveDetailsT
      { fareParametersId = fareParametersId,
        deadKmFare = roundToIntegral $ (Domain.deadKmFare :: Domain.FParamsProgressiveDetails -> HighPrecMoney) fParamsProgressiveDetails,
        extraKmFare = roundToIntegral <$> Domain.extraKmFare fParamsProgressiveDetails,
        deadKmFareAmount = Just $ (Domain.deadKmFare :: Domain.FParamsProgressiveDetails -> HighPrecMoney) fParamsProgressiveDetails,
        extraKmFareAmount = Domain.extraKmFare fParamsProgressiveDetails,
        rideDurationFare = Domain.rideDurationFare fParamsProgressiveDetails,
        currency = Just fParamsProgressiveDetails.currency
      }
