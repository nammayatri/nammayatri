{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Issue.IssueCategory where

import qualified Domain.Types.Issue.IssueCategory as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    IssueCategoryT sql=issue_category
      id Text
      category Text
      logoUrl Text
      Primary id
      deriving Generic
    |]
    
instance TEntityKey IssueCategoryT where
  type DomainKey IssueCategoryT = Id Domain.IssueCategory
  fromKey (IssueCategoryTKey id) = Id id
  toKey (Id id) = IssueCategoryTKey id

instance TType IssueCategoryT Domain.IssueCategory where
  fromTType IssueCategoryT {..} = do
    return $
      Domain.IssueCategory 
        { id = Id id,
          ..
        }
  toTType Domain.IssueCategory {..} =
    IssueCategoryT
      { id = getId id,
        ..
      }
