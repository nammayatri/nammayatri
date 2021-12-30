{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Search where

import Beckn.Prelude
import Beckn.Types.Id

data Person

data Search = Search
  { id :: Id Search,
    lat :: Double,
    lon :: Double,
    requestorId :: Id Person,
    createdAt :: UTCTime
  }
  deriving (Generic)

data SearchAPIEntity = SearchAPIEntity
  { id :: Id Search,
    lat :: Double,
    lon :: Double,
    requestorId :: Id Person
  }
  deriving (Generic, ToJSON)

makeSearchAPIEntity :: Search -> SearchAPIEntity
makeSearchAPIEntity Search {..} = do
  SearchAPIEntity
    { ..
    }