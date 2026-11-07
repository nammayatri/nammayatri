{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Main where

import Common.Types.App (GlobalPayload, FCMBundleUpdate, Event)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (new)
import Engineering.Helpers.Commons (flowRunner, getWindowVariable, liftFlow, markPerformance)
import Flow as Flow
import Helpers.Version
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Generic (decode)
import JBridge as JBridge
import Log (printLog)
import ModifyScreenState (modifyScreenState)
import Prelude (Unit, bind, pure, show, unit, void, ($), (<$>), (<<<), discard, (==))
import Presto.Core.Types.Language.Flow (throwErr)
import PrestoDOM.Core (processEvent) as PrestoDom
import Types.App (defaultGlobalState, FlowBT, ScreenType(..))
import Screens.Types(PermissionScreenStage(..), SafetyAlertType(..), FareProductType(..))
import AssetsProvider (fetchAssets)
import Effect.Uncurried
import Engineering.Helpers.BackTrack (liftFlowBT)
import Storage (setValueToLocalStore, KeyStore(..))

main :: Event -> Boolean -> Effect Unit
main event callInitUI = do
  void $ markPerformance "MAIN_FLOW"
  JBridge.cleanOnResumeCallback
  JBridge.cleanOnPauseCallback
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ getWindowVariable "__payload" Just Nothing)
  case payload of
    Right payload'  -> do
      _ <- launchAff $ flowRunner defaultGlobalState $ do
          _ <- runExceptT $ runBackT $ updateEventData event
          resp ← runExceptT $ runBackT $ Flow.baseAppFlow payload' callInitUI
          case resp of
                Right x → pure unit
                Left err → do
                  _ <- pure $ printLog "printLog error in main is : " err
                  _ <- liftFlow $ main event callInitUI
                  pure unit
      -- required if we need to update Payment page assets in first run
      _ <- launchAff $ flowRunner defaultGlobalState $ fetchAssets 
      pure unit
    Left e -> do
        _ <- launchAff $ flowRunner defaultGlobalState $ do
            throwErr $ show e
        pure unit

onEvent :: String -> Effect Unit
onEvent event = do
  _ <- pure $ JBridge.toggleBtnLoader "" false
  case event of
    "onBackPressed" -> do
      PrestoDom.processEvent "onBackPressedEvent" unit
    "onReloadApp" -> main {type : "", data : ""} false
    _ -> pure unit

onConnectivityEvent :: String -> Effect Unit
onConnectivityEvent triggertype = do
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ getWindowVariable "__payload" Just Nothing)
  case payload of
    Right payload'  -> do
        _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $
                do
                case triggertype of
                  "LOCATION_DISABLED" -> do
                    modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = LOCATION_DISABLED})
                    Flow.permissionScreenFlow
                  "INTERNET_ACTION" -> pure unit
                  "REFRESH" -> Flow.baseAppFlow payload' false
                  _ -> Flow.baseAppFlow payload' false
                pure unit
        pure unit
    Left e -> do
        _ <- launchAff $ flowRunner defaultGlobalState $ do
          throwErr $ show e
        pure unit

updateEventData :: Event -> FlowBT String Unit
updateEventData event = do
    case event.type of
      "CHAT_MESSAGE" -> do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ openChatScreen = true } })
      "REFERRER_URL" -> setValueToLocalStore REFERRER_URL event.data
      "SAFETY_ALERT_DEVIATION" -> do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ safetyAlertType = if homeScreen.data.fareProductType == DELIVERY then Nothing else Just DEVIATION } })
      _ -> pure unit

onNewIntent :: Event -> Effect Unit
onNewIntent event = do
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ getWindowVariable "__payload" Just Nothing)
  case payload of
    Right payload'  -> do
        _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $
                case event.type of
                  "REFERRAL" -> do
                    setValueToLocalStore REFERRER_URL event.data
                    Flow.baseAppFlow payload' false
                  "REFERRAL_NEW_INTENT" -> do
                    setValueToLocalStore REFERRER_URL event.data
                    Flow.baseAppFlow payload' true
                  _ -> Flow.baseAppFlow payload' false
        pure unit
    Left e -> do
        _ <- launchAff $ flowRunner defaultGlobalState $ do
          throwErr $ show e
        pure unit

onBundleUpdatedEvent :: FCMBundleUpdate -> Effect Unit
onBundleUpdatedEvent description= do
  _ <- launchAff $ flowRunner defaultGlobalState $ do
    _ ← runExceptT $ runBackT $ do
      appUpdatedFlow description
    pure unit
  pure unit

mockFollowRideEvent :: Event -> Effect Unit
mockFollowRideEvent event = do
  void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
    Flow.updateFollower true true $ Just event.type