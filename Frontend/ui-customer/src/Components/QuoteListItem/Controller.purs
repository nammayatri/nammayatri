module Components.QuoteListItem.Controller where

import Data.Maybe (Maybe(..))

data Action = Click QuoteListItemState
              | NoAction 
              | CountDown Int String String String 
              | ConfirmRide
              | CancelAutoAssigning

type QuoteListItemState = 
  {
    seconds :: Int
  , id :: String  
  , timer :: String
  , timeLeft :: Int
  , driverRating :: Number
  , profile :: String
  , price :: String
  , vehicleType :: String
  , driverName :: String
  , selectedQuote :: Maybe String
  }

config :: QuoteListItemState
config = {
   seconds : 15
  , id : ""  
  , timer : "-"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "0"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  , selectedQuote : Nothing
  }