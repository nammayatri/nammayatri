module Core.Intent where

import Beckn.Prelude
import Beckn.Utils.Example
import Core.Fulfillment

newtype Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Intent where
  example =
    Intent
      { fulfillment = example
      }

exampleItent :: Intent
exampleItent = example
