{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Types.Actor
  ( JourneyActor (..),
    actorCachePrefix,
  )
where

import Kernel.Prelude

-- | Distinguishes driver vs rider Redis namespaces so the same person/journey
-- IDs in atlas_driver_offer_bpp and atlas_app never collide in shared Redis.
data JourneyActor
  = DriverActor
  | RiderActor
  deriving (Eq, Show, Generic)

actorCachePrefix :: JourneyActor -> Text
actorCachePrefix = \case
  DriverActor -> "driver-offer"
  RiderActor -> "rider-app"
