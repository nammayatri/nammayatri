module SharedLogic.SharedCab.Plate
  ( canonicalisePlate,
    samePlate,
    findByPlate,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as T
import Kernel.Prelude

-- | The single stored/compared form of a plate: uppercase, no whitespace, hyphens or dots.
-- Apply at every write of a vehicle number so "ml 05-a 1234" and "ML05A1234" are one cab.
canonicalisePlate :: Text -> Text
canonicalisePlate = T.toUpper . T.filter keep
  where
    keep c = not (Char.isSpace c) && c /= '-' && c /= '.'

-- | The one way to match a session to an LTS position (or any other plate source): never compare raw plates.
samePlate :: Text -> Text -> Bool
samePlate a b = canonicalisePlate a == canonicalisePlate b

-- | The entry carrying the plate, whatever its spelling.
findByPlate :: (a -> Text) -> Text -> [a] -> Maybe a
findByPlate plateOf plate = find (samePlate plate . plateOf)
