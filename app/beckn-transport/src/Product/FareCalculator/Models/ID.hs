module Product.FareCalculator.Models.ID where

import EulerHS.Prelude

newtype ID domain = ID Text deriving (Show, Eq)

getId :: ID a -> Text
getId (ID a) = a
