{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Search where

import Beckn.Prelude
import Beckn.Types.Id

data BAPPerson

data Search = Search
  { id :: Id Search,
    lat :: Double,
    lon :: Double,
    requestorId :: Id BAPPerson,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    createdAt :: UTCTime
  }
