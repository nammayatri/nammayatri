module Components.TutorialModal.Controller where

data Action = OnCloseClick | CallSupport | Logout

type State =
    {
        imageUrl :: String
    }