module Storage.Queries.Issue.IssueTranslation where

import Domain.Types.Issue.IssueTranslation
import qualified Kernel.Beam.Types as KBT
import Kernel.Types.Id
import qualified Storage.Beam.Issue.IssueTranslation as BeamIT

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
