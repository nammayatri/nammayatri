module Beckn.Utils.Mock
  ( noSearchResultId,
    serviceUnavailableId,
    locationTooFarId,
  )
where

import EulerHS.Prelude

noSearchResultId :: Text
noSearchResultId = "err-0001-no-search-results"

serviceUnavailableId :: Text
serviceUnavailableId = "err-0002-service-unavailable"

locationTooFarId :: Text
locationTooFarId = "err-0003-location-too-far"