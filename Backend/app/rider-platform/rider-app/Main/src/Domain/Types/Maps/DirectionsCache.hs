{-# LANGUAGE DerivingVia #-}

module Domain.Types.Maps.DirectionsCache where

import Data.Text
import Kernel.External.Maps (RouteInfo)
import Kernel.Prelude
import Kernel.Types.Id

data DirectionsCache = DirectionsCache
  { id :: Id DirectionsCache,
    originHash :: Text,
    destHash :: Text,
    slot :: Int,
    response :: RouteInfo,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
