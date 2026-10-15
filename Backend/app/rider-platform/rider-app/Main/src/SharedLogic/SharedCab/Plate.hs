module SharedLogic.SharedCab.Plate
  ( canonicalisePlate,
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
