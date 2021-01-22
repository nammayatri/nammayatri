module Beckn.Types.ID where

import qualified Data.Text as Text
import EulerHS.Prelude

newtype ID domain = ID Text deriving (Show, Eq)

getId :: ID a -> Text
getId (ID a) = a

instance IsString (ID d) where
  fromString = ID . Text.pack
