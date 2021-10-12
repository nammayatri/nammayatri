module Beckn.Utils.NonEmpty where

import Data.List.NonEmpty (NonEmpty (..))

singleton :: a -> NonEmpty a
singleton a = a :| []
