module Screens.DeleteAccountScreen.ScreenData where

initData :: State
initData =
  { data:
      { email: ""
      , description: ""
      }
  , props:
      { showConfirmPopUp: false
      , showRequestSubmitted: false
      }
  }

type State
  = { data :: Data, props :: Props }

type Data
  = { email :: String
    , description :: String
    }

type Props
  = { showConfirmPopUp :: Boolean
    , showRequestSubmitted :: Boolean
    }
