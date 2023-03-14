module Components.SelectMenuButton.Controller where

data Action = OnSelection State

type State = { 
      text :: Text , 
      isSelected :: Boolean ,
      index :: Int
      }
type Text = { 
    name :: String, 
    value :: String, 
    subtitle :: String
    }
