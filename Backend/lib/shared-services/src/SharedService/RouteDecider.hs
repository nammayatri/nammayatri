module SharedService.RouteDecider where

import Kernel.Prelude
import Kernel.Types.Common (Meters, Seconds)

dataDecider :: Seconds -> NonEmpty (Meters, Seconds) -> (Meters, Seconds)
dataDecider threshold (x :| xs) = foldl' decider x xs
  where
    decider (distance1, duration1) (distance2, duration2) = do
      let durationDiff = abs $ duration1 - duration2

      -- If durationDiff is within threshold then choose shorter route
      if durationDiff <= threshold
        then
          if distance1 < distance2
            then (distance1, duration1)
            else (distance2, duration2)
        else -- Otherwise choose faster route

          if duration1 < duration2
            then (distance1, duration1)
            else (distance2, duration2)
