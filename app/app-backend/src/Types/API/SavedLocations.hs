module Types.API.SavedLocations
  ( module Types.API.SavedLocations,
    module Reexport,
  )
where

import Data.OpenApi (ToSchema)
import Domain.Types.SavedReqLocation as Reexport
import EulerHS.Prelude hiding (id)

newtype SavedLocationsListRes = SavedLocationsListRes
  { list :: [SavedReqLocationAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makeSavedReqLocationAPIEntityList :: [SavedReqLocationAPIEntity] -> SavedLocationsListRes
makeSavedReqLocationAPIEntityList savedLocs =
  SavedLocationsListRes
    { list = savedLocs
    }
