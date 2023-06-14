module Storage.Queries.FareParameters.FareParametersProgressiveDetails where

import qualified Domain.Types.FareParameters as Domain
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Lib.Utils (setMeshConfig)
import Sequelize as Se
import Storage.Beam.FareParameters.FareParametersProgressiveDetails as BeamFPPD
import qualified Storage.Tabular.FareParameters.FareParametersProgressiveDetails as DomainFPPD

findById' :: L.MonadFlow m => KTI.Id Domain.FareParameters -> m (Maybe DomainFPPD.FullFareParametersProgressiveDetails)
findById' (KTI.Id fareParametersId') = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamFPPD.FareParametersProgressiveDetailsT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamFareParametersProgressiveDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is fareParametersId $ Se.Eq fareParametersId']
    Nothing -> pure Nothing

transformBeamFareParametersProgressiveDetailsToDomain :: FareParametersProgressiveDetails -> DomainFPPD.FullFareParametersProgressiveDetails
transformBeamFareParametersProgressiveDetailsToDomain FareParametersProgressiveDetailsT {..} = do
  ( KTI.Id fareParametersId,
    Domain.FParamsProgressiveDetails
      { deadKmFare = deadKmFare,
        extraKmFare = extraKmFare
      }
    )

transformDomainFareParametersProgressiveDetailsToBeam :: DomainFPPD.FullFareParametersProgressiveDetails -> FareParametersProgressiveDetails
transformDomainFareParametersProgressiveDetailsToBeam (KTI.Id fareParametersId, Domain.FParamsProgressiveDetails {..}) =
  FareParametersProgressiveDetailsT
    { fareParametersId = fareParametersId,
      deadKmFare = deadKmFare,
      extraKmFare = extraKmFare
    }
