{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Search where

import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.SearchLocation

data Person

data Search = Search
  { id :: Id Search,
    searchLocationId :: Id SearchLocation,
    requestorId :: Id Person,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    createdAt :: UTCTime
  }