module Screens.NammaSafetyFlow.SafetyFlow where

import Data.Maybe
import JBridge
import Prelude
import Screens.Types
import Services.API
import Types.App
import Engineering.Helpers.Utils as EHU
import Control.Monad.Except.Trans (lift)
import Language.Strings (getString)
import Language.Types (STR(..))
import ModifyScreenState (modifyScreenState)
import Screens.Handlers as UI
import Screens.NammaSafetyFlow.SafetyEducationScreen.Controller as SafetyEducationScreen
import Services.Backend as Remote
import Storage (KeyStore(..), setValueToLocalStore)

safetyEducationFlow :: FlowBT String Unit
safetyEducationFlow = do
  void $ pure $ cleverTapCustomEvent "ny_user_safety_learn_more_clicked"
  flow <- UI.safetyEducationScreen
  case flow of
    SafetyEducationScreen.Refresh state -> do
      safetyEducationFlow
    _ -> pure unit
  pure unit

updateEmergencySettings :: NammaSafetyScreenState -> FlowBT String Unit
updateEmergencySettings state = do
  let
    req =
      UpdateEmergencySettingsReq
        { shareEmergencyContacts: Just state.data.shareToEmergencyContacts
        , shareTripWithEmergencyContacts: Just state.data.shareTripWithEmergencyContacts
        , nightSafetyChecks: Just state.data.nightSafetyChecks
        , hasCompletedSafetySetup: Just if state.props.onRide then false else true
        }

    wasSetupAlreadyDone = state.data.hasCompletedSafetySetup
  void $ lift $ lift $ EHU.loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
  void $ lift $ lift $ EHU.toggleLoader true
  void $ lift $ lift $ Remote.updateEmergencySettings req
  modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> state { data { hasCompletedSafetySetup = if state.props.onRide then false else true } })
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { showSosBanner = false } })
  if not state.props.onRide && not wasSetupAlreadyDone then
    pure $ toast $ getString NAMMA_SAFETY_IS_SET_UP
  else
    pure unit
