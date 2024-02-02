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
    SafetyEducationScreen.Refresh _ -> do
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
        , hasCompletedSafetySetup: Just $ not state.props.onRide
        }

    wasSetupAlreadyDone = state.data.hasCompletedSafetySetup
  void $ lift $ lift $ Remote.updateEmergencySettings req
  modifyScreenState $ NammaSafetyScreenStateType (\_ -> state { data { hasCompletedSafetySetup = not state.props.onRide } })
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            homeScreen
              { props
                { sosBannerType = if not state.data.hasCompletedMockSafetyDrill 
                                    then Just MOCK_DRILL_BANNER 
                                    else Nothing
                }
              , data { settingSideBar { hasCompletedSafetySetup = true } }
              }
        )
  if not state.props.onRide && not wasSetupAlreadyDone then
    pure $ toast $ getString NAMMA_SAFETY_IS_SET_UP
  else
    pure unit
