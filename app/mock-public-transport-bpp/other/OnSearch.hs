module Core.OnSearch
  ( module Core.OnSearch,
    module Reexport,
  )
where

import Core.Catalog as Reexport
import Data.Aeson
import GHC.Generics

newtype OnSearchMessage = OnSearchMessage
  { catalog :: Catalog
  }
  deriving (Generic, ToJSON, FromJSON)
