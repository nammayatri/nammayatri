module Domain.Types.ItemId (ItemId (..), parseText, toText) where

import qualified Data.Text as Text
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (toText)

data ItemId
  = RecurringTrip Variant
  | OneTimeTrip Variant

parseText :: Text -> Maybe ItemId
parseText t =
  case Text.splitOn "_" $ Text.toUpper t of
    ["RECURRING", "TRIP", variantText] ->
      Just $ RecurringTrip (read $ Text.unpack variantText)
    ["ONE", "TIME", "TRIP", variantText] ->
      Just $ OneTimeTrip (read $ Text.unpack variantText)
    _ ->
      Nothing

toText :: ItemId -> Text
toText (RecurringTrip variant) = "RECURRING_TRIP_" <> show variant
toText (OneTimeTrip variant) = "ONE_TIME_TRIP_" <> show variant
