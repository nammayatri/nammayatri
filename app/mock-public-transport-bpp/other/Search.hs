module Core.Search
  ( module Core.Search,
    module Reexport,
  )
where

import Core.Intent as Reexport
import Data.Aeson
import GHC.Generics

newtype SearchMessage = SearchMessage
  { indent :: Intent
  }
  deriving (Generic, ToJSON, FromJSON)
