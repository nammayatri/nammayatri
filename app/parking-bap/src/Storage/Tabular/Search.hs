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
import Data.Time (UTCTime)
import Database.Persist.TH
import qualified Domain.Search as Domain
import Storage.Tabular.SearchLocation (SearchLocationTId)

share
  [mkPersist defaultSqlSettings]
  [defaultQQ|
    SearchT sql=search
      id Text
      searchLocationId SearchLocationTId
      requestorId Text
      fromDate UTCTime
      toDate UTCTime
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchT Domain.Search where
  fromKey (SearchTKey _id) = Id _id
  toKey id = SearchTKey id.getId

instance TEntity SearchT Domain.Search where
  fromTEntity entity = do
    let SearchT {..} = entityVal entity
    return $
      Domain.Search
        { id = Id id,
          searchLocationId = fromKey searchLocationId,
          requestorId = Id requestorId,
          ..
        }
  toTType Domain.Search {..} = do
    SearchT
      { id = id.getId,
        searchLocationId = toKey searchLocationId,
        requestorId = requestorId.getId,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a