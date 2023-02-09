module Storage.Queries.Issues (insertIssue) where

import Domain.Types.Issue
import Kernel.Storage.Esqueleto
import Storage.Tabular.Issue ()

insertIssue :: Issue -> SqlDB ()
insertIssue = do
  create
