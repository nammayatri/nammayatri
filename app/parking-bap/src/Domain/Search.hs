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
    fromDate :: UTCTime,
    toDate :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)
