{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Metro.OnSearch.Vehicle where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Vehicle = Vehicle
  { category :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
