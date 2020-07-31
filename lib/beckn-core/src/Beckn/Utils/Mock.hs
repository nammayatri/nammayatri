module Beckn.Utils.Mock
  ( noSearchResultId,
    searchPickupLocationNotServiceableId,
  )
where

import EulerHS.Prelude

noSearchResultId :: Text
noSearchResultId = "err-0001-no-search-results"

searchPickupLocationNotServiceableId :: Text
searchPickupLocationNotServiceableId = "search-error-fmd-001"
