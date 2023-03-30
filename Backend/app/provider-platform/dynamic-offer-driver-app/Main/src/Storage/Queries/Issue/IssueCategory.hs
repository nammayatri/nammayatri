module Storage.Queries.Issue.IssueCategory where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Issue.IssueCategory
import Storage.Tabular.Issue.IssueTranslation

fullCategoryTable ::
  Language ->
  From
    ( Table IssueCategoryT
        :& MbTable IssueTranslationT
    )
fullCategoryTable language =
  table @IssueCategoryT
    `leftJoin` table @IssueTranslationT
      `Esq.on` ( \(category :& translation) ->
                   just (category ^. IssueCategoryCategory) ==. translation ?. IssueTranslationSentence
                     &&. translation ?. IssueTranslationLanguage ==. just (val language)
               )

findAllByLanguage :: Transactionable m => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findAllByLanguage language = Esq.findAll $ do
  (issueCategory :& mbIssueTranslation) <- from $ fullCategoryTable language
  return (issueCategory, mbIssueTranslation)

findById :: Transactionable m => Id IssueCategory -> m (Maybe IssueCategory)
findById issueCategoryId = Esq.findOne $ do
  issueCategory <- from $ table @IssueCategoryT
  where_ $ issueCategory ^. IssueCategoryTId ==. val (toKey issueCategoryId)
  return issueCategory

findByIdAndLanguage :: Transactionable m => Id IssueCategory -> Language -> m (Maybe (IssueCategory, Maybe IssueTranslation))
findByIdAndLanguage issueCategoryId language = Esq.findOne $ do
  (issueCategory :& mbIssueTranslation) <- from $ fullCategoryTable language
  where_ $ issueCategory ^. IssueCategoryTId ==. val (toKey issueCategoryId)
  return (issueCategory, mbIssueTranslation)
