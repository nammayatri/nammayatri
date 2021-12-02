{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Data.Time (UTCTime)
import Database.Persist.TH
import qualified Domain.SearchLocation as Domain

share
  [mkPersist defaultSqlSettings]
  [defaultQQ|
    SearchLocationT sql=search_location
      id Text
      lat Double
      lon Double
      createdAt UTCTime
      deriving Generic
      Primary id
    |]

instance TEntityKey SearchLocationT Domain.SearchLocation where
  fromKey (SearchLocationTKey _id) = Id _id
  toKey id = SearchLocationTKey id.getId

instance TEntity SearchLocationT Domain.SearchLocation where
  fromTEntity entity = do
    let SearchLocationT {..} = entityVal entity
    return $
      Domain.SearchLocation
        { id = Id id,
          ..
        }
  toTType Domain.SearchLocation {..} = do
    SearchLocationT
      { id = id.getId,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a