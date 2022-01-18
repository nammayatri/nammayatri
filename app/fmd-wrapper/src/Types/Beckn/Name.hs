{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Beckn.Name where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Name = Name
  { getName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema)
