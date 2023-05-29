module Storage.Queries.FarePolicy.FarePolicyProgressiveDetails where

import qualified Domain.Types.FarePolicy as Domain
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import qualified Lib.Mesh as Mesh
import Sequelize as Se
import Storage.Beam.FarePolicy.FarePolicyProgressiveDetails
import qualified Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails as DomainFPPD

findById' :: L.MonadFlow m => KTI.Id Domain.FarePolicy -> m (Maybe DomainFPPD.FullFarePolicyProgressiveDetails)
findById' (KTI.Id farePolicyId') = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamFarePolicyProgressiveDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is farePolicyId $ Se.Eq farePolicyId']
    Nothing -> pure Nothing

transformBeamFarePolicyProgressiveDetailsToDomain :: FarePolicyProgressiveDetails -> DomainFPPD.FullFarePolicyProgressiveDetails
transformBeamFarePolicyProgressiveDetailsToDomain FarePolicyProgressiveDetailsT {..} = do
  ( KTI.Id farePolicyId,
    Domain.FPProgressiveDetails
      { baseDistance = baseDistance,
        baseFare = baseFare,
        perExtraKmFare = perExtraKmFare,
        deadKmFare = deadKmFare,
        waitingCharge = waitingCharge,
        nightShiftCharge = nightShiftCharge
      }
    )

transformDomainFarePolicyProgressiveDetailsToBeam :: DomainFPPD.FullFarePolicyProgressiveDetails -> FarePolicyProgressiveDetails
transformDomainFarePolicyProgressiveDetailsToBeam (KTI.Id farePolicyId, Domain.FPProgressiveDetails {..}) =
  FarePolicyProgressiveDetailsT
    { farePolicyId = farePolicyId,
      baseDistance = baseDistance,
      baseFare = baseFare,
      perExtraKmFare = perExtraKmFare,
      deadKmFare = deadKmFare,
      waitingCharge = waitingCharge,
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
