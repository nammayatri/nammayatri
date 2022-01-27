module Storage.Queries.Issues (insertIssue) where

import Beckn.Storage.Esqueleto
import Domain.Types.Issue
import Storage.Tabular.Issue ()

insertIssue :: Issue -> SqlDB ()
insertIssue = do
  create'
