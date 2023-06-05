module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList)
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.FarePolicy.Common as Common
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import qualified Lib.Mesh as Mesh
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as BeamFPPD
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QueriesFPPDP
import qualified Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails as DomainFPPD

findById' :: L.MonadFlow m => KTI.Id Domain.FarePolicy -> m (Maybe DomainFPPD.FullFarePolicyProgressiveDetails)
findById' (KTI.Id farePolicyId') = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      res <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamFPPD.farePolicyId $ Se.Eq farePolicyId']
      case res of
        Left _ -> pure Nothing
        Right x -> mapM transformBeamFarePolicyProgressiveDetailsToDomain x
    Nothing -> pure Nothing

--type FullFarePolicyProgressiveDetailsT = (FarePolicyProgressiveDetailsT, [FarePolicyProgressiveDetailsPerExtraKmRateSectionT])

transformBeamFarePolicyProgressiveDetailsToDomain :: (L.MonadFlow m) => BeamFPPD.FarePolicyProgressiveDetails -> m (DomainFPPD.FullFarePolicyProgressiveDetails)
transformBeamFarePolicyProgressiveDetailsToDomain BeamFPPD.FarePolicyProgressiveDetailsT {..} = do
  fullFPPDP <- QueriesFPPDP.findAll (KTI.Id farePolicyId)
  let fPPDP = snd <$> fullFPPDP
  pure $
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
