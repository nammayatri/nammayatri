{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Issue.IssueTranslation where

import qualified Domain.Types.Issue.IssueTranslation as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.External.Types (Language)

derivePersistField "Language"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    IssueTranslationT sql=issue_translation
      id Text
      sentence Text
      translation Text
      language Language
      Primary id
      deriving Generic
    |]

instance TEntityKey IssueTranslationT where
  type DomainKey IssueTranslationT = Id Domain.IssueTranslation
  fromKey (IssueTranslationTKey id) = Id id
  toKey (Id id) = IssueTranslationTKey id

instance TType IssueTranslationT Domain.IssueTranslation where
  fromTType IssueTranslationT {..} = do
    return $
      Domain.IssueTranslation 
        { id = Id id,
          ..
        }
  toTType Domain.IssueTranslation {..} =
    IssueTranslationT
      { id = getId id,
        ..
      }
