module Core.Intent where

import Beckn.Utils.Example
import Core.Fulfillment

import Data.Aeson
import GHC.Generics

data Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)
instance Example Intent where
  example =
    Intent
      { fulfillment = example       
      } 
        
example_itent :: Intent
example_itent = example

