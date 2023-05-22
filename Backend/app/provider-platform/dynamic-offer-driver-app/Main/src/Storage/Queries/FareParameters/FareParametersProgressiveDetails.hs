module Storage.Queries.FareParameters.FareParametersProgressiveDetails where

import qualified Domain.Types.FareParameters as Domain
import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueTranslation
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Kernel.Types.Id as KTI
import qualified Lib.Mesh as Mesh
import Lib.UtilsTH
import Sequelize as Se
import qualified Sequelize as Se
import Storage.Beam.FareParameters.FareParametersProgressiveDetails
import qualified Storage.Tabular.FareParameters.FareParametersProgressiveDetails as DomainFPPD

findById' :: L.MonadFlow m => KTI.Id Domain.FareParameters -> m (Maybe DomainFPPD.FullFareParametersProgressiveDetails)
findById' (KTI.Id fareParametersId') = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamFareParametersProgressiveDetailsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is fareParametersId $ Se.Eq fareParametersId']
    Nothing -> pure Nothing

transformBeamFareParametersProgressiveDetailsToDomain :: FareParametersProgressiveDetails -> DomainFPPD.FullFareParametersProgressiveDetails
transformBeamFareParametersProgressiveDetailsToDomain FareParametersProgressiveDetailsT {..} = do
  ( (KTI.Id fareParametersId),
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
