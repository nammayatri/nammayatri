{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Issue.IssueOption
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Issue.IssueOption
import Storage.Tabular.Issue.IssueTranslation

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
