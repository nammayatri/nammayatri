module Storage.Queries.Issue.IssueCategory where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueTranslation
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Issue.IssueCategory
import Storage.Tabular.Issue.IssueTranslation
import Kernel.External.Types (Language)

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
                   just(category ^. IssueCategoryCategory) ==. translation ?. IssueTranslationSentence
                   &&. translation ?. IssueTranslationLanguage ==. just(val language)
               )

findByLanguage :: Transactionable m => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findByLanguage language = Esq.findAll $ do
  (issueCategory :& mbIssueTranslation) <- from $ fullCategoryTable language
  return (issueCategory, mbIssueTranslation)