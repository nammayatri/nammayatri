{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails where

import Data.List.NonEmpty (nonEmpty)
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.FarePolicy.Common as Common
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import qualified Lib.Mesh as Mesh
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QueriesFPPDP
import qualified Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails as DomainFPPD

findById' :: L.MonadFlow m => KTI.Id Domain.FarePolicy -> m (Maybe DomainFPPD.FullFarePolicyProgressiveDetails)
findById' (KTI.Id farePolicyId') = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      res <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamFPPD.farePolicyId $ Se.Eq farePolicyId']
      case res of
        Left _ -> pure Nothing
        Right x -> mapM transformBeamFarePolicyProgressiveDetailsToDomain x
    Nothing -> pure Nothing

--type FullFarePolicyProgressiveDetailsT = (FarePolicyProgressiveDetailsT, [FarePolicyProgressiveDetailsPerExtraKmRateSectionT])

transformBeamFarePolicyProgressiveDetailsToDomain :: (L.MonadFlow m) => BeamFPPD.FarePolicyProgressiveDetails -> m DomainFPPD.FullFarePolicyProgressiveDetails
transformBeamFarePolicyProgressiveDetailsToDomain BeamFPPD.FarePolicyProgressiveDetailsT {..} = do
  fullFPPDP <- QueriesFPPDP.findAll (KTI.Id farePolicyId)
  let fPPDP = snd <$> fullFPPDP
  pure
    ( KTI.Id farePolicyId,
      Domain.FPProgressiveDetails
        { baseDistance = baseDistance,
          baseFare = baseFare,
          perExtraKmRateSections = fromJust $ nonEmpty fPPDP,
          -- perExtraKmFare = perExtraKmFare,
          -- freeWatingTime = freeWatingTime,
          -- FPProgressiveDetailsD
          deadKmFare = deadKmFare,
          -- waitingCharge = waitingCharge,
          waitingChargeInfo =
            ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
              Domain.WaitingChargeInfo
                { waitingCharge = waitingCharge',
                  freeWaitingTime = freeWaitingTime'
                },
          nightShiftCharge = nightShiftCharge
        } --fPPDP
    )

transformDomainFarePolicyProgressiveDetailsToBeam :: DomainFPPD.FullFarePolicyProgressiveDetails -> BeamFPPD.FarePolicyProgressiveDetails
transformDomainFarePolicyProgressiveDetailsToBeam (KTI.Id farePolicyId, Domain.FPProgressiveDetails {..}) =
  BeamFPPD.FarePolicyProgressiveDetailsT
    { farePolicyId = farePolicyId,
      baseDistance = baseDistance,
      baseFare = baseFare,
      -- perExtraKmFare = perExtraKmFare,
      freeWatingTime = freeWaitingTime <$> waitingChargeInfo,
      deadKmFare = deadKmFare,
      waitingCharge = Common.waitingCharge <$> waitingChargeInfo,
      nightShiftCharge = nightShiftCharge
    }

-- transformBeamFareParametersProgressiveDetailsToDomain :: FareParametersProgressiveDetails -> DomainFPPD.FullFareParametersProgressiveDetails
-- transformBeamFareParametersProgressiveDetailsToDomain FareParametersProgressiveDetailsT {..} = do
--   ( (KTI.Id fareParametersId),
--     Domain.FParamsProgressiveDetails
--       { deadKmFare = deadKmFare,
--         extraKmFare = extraKmFare
--       }
--     )

-- transformDomainFareParametersProgressiveDetailsToBeam :: DomainFPPD.FullFareParametersProgressiveDetails -> FareParametersProgressiveDetails
-- transformDomainFareParametersProgressiveDetailsToBeam (KTI.Id fareParametersId, Domain.FParamsProgressiveDetails {..}) =
--   FareParametersProgressiveDetailsT
--     {
--         farePolicyId = farePolicyId,
--         baseDistance = baseDistance,
--         baseFare = baseFare,
--         perExtraKmFare = perExtraKmFare,
--         deadKmFare = deadKmFare,
--         waitingCharge = waitingCharge,
--         nightShiftCharge = nightShiftCharge
--     }
