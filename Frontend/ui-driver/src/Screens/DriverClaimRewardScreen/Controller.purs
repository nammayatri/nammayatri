module Screens.DriverClaimRewardScreen.Controller
  ( Action(..)
  , ScreenOutput(..)
  , eval
  )
  where

import Prelude (class Show, pure, unit, ($), not, void,discard ,(<>) ,show , (==))
import PrestoDOM (Eval, update, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (DriverClaimRewardScreenState)
import JBridge (openUrlInApp)
import Storage (getValueToLocalStore, KeyStore(..))
import Helpers.Utils (contactSupportNumber)
import Data.Maybe
import Effect.Unsafe (unsafePerformEffect)

instance showAction :: Show Action where
  show BackPressed = "BackPressed"
  show ViewMoreBenefits = "ViewMoreBenefits"
  show ViewMoreEligibility = "ViewMoreEligibility"
  show (YoutubeVideoStatus _) = "YoutubeVideoStatus"
  show OpenWhatsAppSupport = "OpenWhatsAppSupport"
  show NoAction = "NoAction"
  show FaqClicked = "FaqClicked"
  show CallSupport = "CallSupport"
  show (FaqToggle i) = "FaqToggle " <> show i
  show (BenefitToggle i) = "BenefitToggle" <> show i
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> pure unit
    ViewMoreBenefits -> pure unit
    ViewMoreEligibility -> pure unit
    YoutubeVideoStatus _ -> pure unit
    OpenWhatsAppSupport -> pure unit
    NoAction -> pure unit
    FaqClicked -> pure unit
    FaqToggle _ -> pure unit
    CallSupport -> pure unit
    BenefitToggle _ -> pure unit
    _ -> pure unit

data Action = BackPressed | ViewMoreBenefits | ViewMoreEligibility | YoutubeVideoStatus String | NoAction | OpenWhatsAppSupport | FaqClicked | FaqToggle Int | CallSupport | BenefitToggle Int

data ScreenOutput = GoBack

eval :: Action -> DriverClaimRewardScreenState -> Eval Action ScreenOutput DriverClaimRewardScreenState

eval BackPressed state = if state.props.showFaq then continue state { props { showFaq = false }} else exit GoBack

eval ViewMoreBenefits state = continue state { props { showAllBenefits = not state.props.showAllBenefits } }

eval ViewMoreEligibility state = continue state { props { showAllEligibility = not state.props.showAllEligibility } }

eval OpenWhatsAppSupport state = continueWithCmd state [do
  let phone = "%0APhone%20Number%3A%20" <> getValueToLocalStore MOBILE_NUMBER_KEY
      message = "Hi%20Team%2C%0AI%20would%20require%20help%20in%20onboarding" <> phone
      url = "https://chat.whatsapp.com/INvTBLDhoWG3oCqkOLnWEH?text=" <> message
  void $ openUrlInApp url
  pure NoAction
]

eval FaqClicked state = continue state {props {showFaq = true}}

eval CallSupport state = do
  void $ pure $ unsafePerformEffect $ contactSupportNumber ""
  continue state

eval (FaqToggle i) state =
  let current = state.props.openFaqIndex
      newIndex = if current == Just i then Nothing else Just i
  in continue state { props { openFaqIndex = newIndex } }

eval (BenefitToggle i) state =
  let current = state.props.openBenefitIndex
      newIndex = if current == Just i then Nothing else Just i
  in continue state { props { openBenefitIndex = newIndex } }



eval _ state = update state 