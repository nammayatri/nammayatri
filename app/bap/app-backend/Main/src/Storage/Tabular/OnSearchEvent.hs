{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.OnSearchEvent where

import qualified Domain.Types.OnSearchEvent as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OnSearchEventT sql=on_search_event
      id Text
      bppId Text
      messageId Text
      errorCode Text Maybe
      errorType Text Maybe
      errorMessage Text Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey OnSearchEventT where
  type DomainKey OnSearchEventT = Id Domain.OnSearchEvent
  fromKey (OnSearchEventTKey _id) = Id _id
  toKey (Id id) = OnSearchEventTKey id

instance TType OnSearchEventT Domain.OnSearchEvent where
  fromTType OnSearchEventT {..} = do
    return $
      Domain.OnSearchEvent
        { id = Id id,
          ..
        }
  toTType Domain.OnSearchEvent {..} =
    OnSearchEventT
      { id = getId id,
        ..
      }
