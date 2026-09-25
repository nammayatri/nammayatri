module Components.RideRequestCard.Controller where

import Prelude (class Show)

instance showAction :: Show Action where
  show (Select _) = "Select"

data Action = Select Int