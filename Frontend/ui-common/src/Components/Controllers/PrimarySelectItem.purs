module Components.PrimarySelectItem.Controller where

data Action = OnClick PrimarySelectItemState

type PrimarySelectItemState = {
    label :: String
  , placeholder :: String
  , selectedItem :: String
  , screenName :: String
  }