module Storage.Queries.Transformers.PersonRidePatternSummary where

import Data.Aeson (Value)
import Kernel.Prelude
import Kernel.Utils.JSON (valueToMaybe)

decodeOrEmpty :: (FromJSON a, Monoid a) => Value -> a
decodeOrEmpty = fromMaybe mempty . valueToMaybe
