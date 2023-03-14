module Storage.Queries.Issue.IssueOption where

import Domain.Types.Issue.IssueTranslation
import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Issue.IssueTranslation
import Storage.Tabular.Issue.IssueOption
import Kernel.External.Types (Language)

fullOptionTable ::
  Language ->
  From
    ( Table IssueOptionT
        :& MbTable IssueTranslationT
    )
fullOptionTable language =
  table @IssueOptionT
    `leftJoin` table @IssueTranslationT
      `Esq.on` ( \(option :& translation) ->
                   just(option ^. IssueOptionOption) ==. translation ?. IssueTranslationSentence
                   &&. translation ?. IssueTranslationLanguage ==. just(val language)
               )

findAllByCategoryAndLanguage :: Transactionable m => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage issueCategoryId language = Esq.findAll $ do
  (issueOption :& mbIssueTranslation) <- from $ fullOptionTable language
  where_ $
      issueOption ^. IssueOptionIssueCategoryId ==. val (toKey issueCategoryId)
  pure (issueOption, mbIssueTranslation)