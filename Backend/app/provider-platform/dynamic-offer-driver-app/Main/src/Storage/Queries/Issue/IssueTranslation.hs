module Storage.Queries.Issue.IssueTranslation where

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
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueTranslation as BeamIT
import qualified Storage.Tabular.VechileNew as VN

transformBeamIssueTranslationToDomain :: BeamIT.IssueTranslation -> IssueTranslation
transformBeamIssueTranslationToDomain BeamIT.IssueTranslationT {..} = do
  IssueTranslation
    { id = Id id,
      sentence = sentence,
      translation = translation,
      language = language
    }

transformDomainIssueTranslationToBeam :: IssueTranslation -> BeamIT.IssueTranslation
transformDomainIssueTranslationToBeam IssueTranslation {..} =
  BeamIT.IssueTranslationT
    { id = getId id,
      sentence = sentence,
      translation = translation,
      language = language
    }
