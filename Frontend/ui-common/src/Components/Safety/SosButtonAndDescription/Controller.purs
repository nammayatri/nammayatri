module Components.Safety.SosButtonAndDescription.Controller where


import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Prelude (class Eq, class Show)
import Common.Types.App (RateCardType(..), FareList(..), WaitingTimeInfo(..))
import Components.PrimaryButton as PrimaryButton
import Data.Maybe(Maybe(..))
import PrestoDOM (Length(..), Margin(..), Visibility(..))
import Styles.Colors as Color
import Components.Safety.Utils (MeasureViewConfig)

instance showAction :: Show Action where
  show (NoAction) = "NoAction"
  show (CountDown _ _ _) = "CountDown"
  show (TriggerSosCountdown) = "TriggerSosCountdown"
  show (AddContacts) = "AddContacts"

data Action = NoAction | CountDown Int String String | TriggerSosCountdown | AddContacts


type Config = {
    triggeringSos :: Boolean,
    showTestDrill :: Boolean,
    timerValue :: Int,
    sosDescription :: Array MeasureViewConfig,
    primaryContactAndEdit :: Maybe MeasureViewConfig,
    buttonText :: String,
    descriptionText :: String,
    showSosButton :: Boolean,
    isDisabled :: Boolean,
    editContactText :: String
}

config :: Config 
config = {
    triggeringSos: false,
    showTestDrill: false,
    timerValue: 0,
    sosDescription: [],
    primaryContactAndEdit: Nothing,
    buttonText: "",
    descriptionText: "",
    showSosButton: false,
    isDisabled: false,
    editContactText: ""
}
