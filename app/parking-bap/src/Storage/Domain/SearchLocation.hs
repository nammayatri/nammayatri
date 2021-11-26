{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Domain.SearchLocation
  ( module Storage.Domain.SearchLocation,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Data.Time (UTCTime)
import Storage.Tabular.SearchLocation
import Storage.Tabular.SearchLocation as Reexport (SearchLocationT)
import Storage.Tabular.SearchLocation as Reexport hiding (SearchLocationT (..))

data SearchLocation = SearchLocation
  { id :: Id SearchLocation,
    lat :: Double,
    lon :: Double,
    createdAt :: UTCTime
  }

instance TEntityKey SearchLocationT SearchLocation where
  fromKey (SearchLocationTKey _id) = Id _id
  toKey SearchLocation {id} = SearchLocationTKey id.getId

instance TEntity SearchLocationT SearchLocation where
  fromTEntity entity = do
    let id = fromKey $ entityKey entity
        SearchLocationT {..} = entityVal entity
    return $
      SearchLocation
        { id = id,
          lat = searchLocationTLat,
          lon = searchLocationTLon,
          createdAt = searchLocationTCreatedAt
        }
  toTType SearchLocation {..} = do
    SearchLocationT
      { searchLocationTLat = lat,
        searchLocationTLon = lon,
        searchLocationTCreatedAt = createdAt
      }
  toTEntity a = do
    Entity (toKey a) $ toTType a