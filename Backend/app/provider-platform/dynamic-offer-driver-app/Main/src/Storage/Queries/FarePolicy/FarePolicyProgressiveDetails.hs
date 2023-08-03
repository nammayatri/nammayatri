{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails where

import Data.List.NonEmpty (nonEmpty)
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.FarePolicy.Common as Common
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Kernel.Types.Logging (Log)
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QueriesFPPDP

findById' :: (L.MonadFlow m, Log m) => KTI.Id Domain.FarePolicy -> m (Maybe Domain.FullFarePolicyProgressiveDetails)
findById' (KTI.Id farePolicyId') = findOneWithKV [Se.Is BeamFPPD.farePolicyId $ Se.Eq farePolicyId']

instance FromTType' BeamFPPD.FarePolicyProgressiveDetails Domain.FullFarePolicyProgressiveDetails where
  fromTType' BeamFPPD.FarePolicyProgressiveDetailsT {..} = do
    fullFPPDP <- QueriesFPPDP.findAll' (KTI.Id farePolicyId)
    let fPPDP = snd <$> fullFPPDP
    pure $
      Just
        ( KTI.Id farePolicyId,
          Domain.FPProgressiveDetails
            { baseDistance = baseDistance,
              baseFare = baseFare,
              perExtraKmRateSections = fromJust $ nonEmpty fPPDP,
              deadKmFare = deadKmFare,
              waitingChargeInfo =
                ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
                  Domain.WaitingChargeInfo
                    { waitingCharge = waitingCharge',
                      freeWaitingTime = freeWaitingTime'
                    },
              nightShiftCharge = nightShiftCharge
            }
        )

instance ToTType' BeamFPPD.FarePolicyProgressiveDetails Domain.FullFarePolicyProgressiveDetails where
  toTType' (KTI.Id farePolicyId, Domain.FPProgressiveDetails {..}) =
    BeamFPPD.FarePolicyProgressiveDetailsT
      { farePolicyId = farePolicyId,
        baseDistance = baseDistance,
        baseFare = baseFare,
        freeWatingTime = freeWaitingTime <$> waitingChargeInfo,
        deadKmFare = deadKmFare,
        waitingCharge = Common.waitingCharge <$> waitingChargeInfo,
        nightShiftCharge = nightShiftCharge
      }
