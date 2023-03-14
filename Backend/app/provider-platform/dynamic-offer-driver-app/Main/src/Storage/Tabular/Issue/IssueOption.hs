{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Issue.IssueOption where

import qualified Domain.Types.Issue.IssueOption as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Issue.IssueCategory (IssueCategoryTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    IssueOptionT sql=issue_option
      id Text
      issueCategoryId IssueCategoryTId
      option Text
      Primary id
      deriving Generic
    |]

instance TEntityKey IssueOptionT where
  type DomainKey IssueOptionT = Id Domain.IssueOption
  fromKey (IssueOptionTKey id) = Id id
  toKey (Id id) = IssueOptionTKey id

instance TType IssueOptionT Domain.IssueOption where
  fromTType IssueOptionT {..} = do
    return $
      Domain.IssueOption 
        { id = Id id,
          issueCategoryId = fromKey issueCategoryId,
          ..
        }
  toTType Domain.IssueOption {..} =
    IssueOptionT
      { id = getId id,
        issueCategoryId = toKey issueCategoryId,
        ..
      }
