module Storage.Queries.FareParameters.FareParametersProgressiveDetails where

import qualified Domain.Types.FareParameters as Domain
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import qualified Lib.Mesh as Mesh
import Sequelize as Se
import Storage.Beam.FareParameters.FareParametersProgressiveDetails
import qualified Storage.Tabular.FareParameters.FareParametersProgressiveDetails as DomainFPPD

findById' :: L.MonadFlow m => KTI.Id Domain.FareParameters -> m (Maybe DomainFPPD.FullFareParametersProgressiveDetails)
findById' (KTI.Id fareParametersId') = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamFareParametersProgressiveDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is fareParametersId $ Se.Eq fareParametersId']
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
