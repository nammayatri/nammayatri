{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Search where

import Beckn.Prelude
import Beckn.Types.Id
import Tools.Auth

data Search = Search
  { id :: Id Search,
    lat :: Double,
    lon :: Double,
    requestorId :: Id Person,
    fromDate :: UTCTime,
    toDate :: UTCTime,
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
