{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Search where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Search as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchT sql=search
      id Text
      lat Double
      lon Double
      requestorId Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchT where
  type DomainKey SearchT = Id Domain.Search
  fromKey (SearchTKey _id) = Id _id
  toKey id = SearchTKey id.getId

instance TType SearchT Domain.Search where
  fromTType SearchT {..} = do
    return $
      Domain.Search
        { id = Id id,
          requestorId = Id requestorId,
          ..
        }
  toTType Domain.Search {..} =
    SearchT
      { id = id.getId,
        requestorId = requestorId.getId,
        ..
      }
