module Components.RegistrationModal.Controller where


import Prelude (pure, unit, class Show)
import PrestoDOM.Types.Core (class Loggable)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
   performLog _ _ = pure unit

data Action = OnCloseClick

type State =
  { 
      openRegistrationModal :: Boolean
  }
