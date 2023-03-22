{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Issue.IssueCategory
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
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

findByLanguage :: Transactionable m => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findByLanguage language = Esq.findAll $ do
  (issueCategory :& mbIssueTranslation) <- from $ fullCategoryTable language
  return (issueCategory, mbIssueTranslation)
