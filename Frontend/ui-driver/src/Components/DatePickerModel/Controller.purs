module Components.DatePickerModel.Controller where

import Common.Types.App (DateObj)

data Action = 
    OnDateSelect Int DateObj 
  | NoAction


type Config = {
    activeIndex :: Int
  , dates :: Array DateObj
  , id :: String
  }
config :: Config
config = {
  activeIndex : 0
  , dates : []
  , id : ""
}