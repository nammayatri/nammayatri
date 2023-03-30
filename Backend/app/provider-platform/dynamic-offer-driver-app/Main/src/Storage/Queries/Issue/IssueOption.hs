module Storage.Queries.Issue.IssueOption where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Issue.IssueOption
import Storage.Tabular.Issue.IssueTranslation

findByIdAndCategoryId :: Transactionable m => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId = Esq.findOne $ do
  issueOption <- from $ table @IssueOptionT
  where_ $
    issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
      &&. issueOption ^. IssueOptionIssueCategoryId ==. val (toKey issueCategoryId)
  pure issueOption

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
                   just (option ^. IssueOptionOption) ==. translation ?. IssueTranslationSentence
                     &&. translation ?. IssueTranslationLanguage ==. just (val language)
               )

findAllByCategoryAndLanguage :: Transactionable m => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage issueCategoryId language = Esq.findAll $ do
  (issueOption :& mbIssueTranslation) <- from $ fullOptionTable language
  where_ $
    issueOption ^. IssueOptionIssueCategoryId ==. val (toKey issueCategoryId)
  pure (issueOption, mbIssueTranslation)

findByIdAndLanguage :: Transactionable m => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
findByIdAndLanguage issueOptionId language = Esq.findOne $ do
  (issueOption :& mbIssueTranslation) <- from $ fullOptionTable language
  where_ $
    issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
  pure (issueOption, mbIssueTranslation)

findById :: Transactionable m => Id IssueOption -> m (Maybe IssueOption)
findById issueOptionId = Esq.findOne $ do
  issueOption <- from $ table @IssueOptionT
  where_ $
    issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
  pure issueOption
