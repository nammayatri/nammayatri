module Beckn.Utils.Example where

import Data.Time
import EulerHS.Prelude

class Example a where
  -- | Sample value of a thing.
  --
  -- This can be used for mocking.
  -- Also, it is especially useful for including examples into swagger,
  -- because random generation can produce non-demostrative values
  -- (e.g. empty lists) unless special care is taken.
  example :: a

instance Example a => Example (Maybe a) where
  example = Just example

instance Example a => Example [a] where
  example = one example

instance Example UTCTime where
  example =
    UTCTime
      (fromGregorian 2020 8 2)
      (timeOfDayToTime (TimeOfDay 7 7 7))

-- until we start using newtypes everywhere
idExample :: Text
idExample = "123e4567-e89b-12d3-a456-426655440000"
