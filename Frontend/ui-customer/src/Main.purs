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
import Engineering.Helpers.Commons (flowRunner, getWindowVariable, liftFlow)
import Flow as Flow
import Engineering.Helpers.Version
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Generic (decode)
import JBridge as JBridge
import Log (printLog)
import ModifyScreenState (modifyScreenState)
import Prelude (Unit, bind, pure, show, unit, void, ($), (<$>), (<<<), discard)
import Presto.Core.Types.Language.Flow (throwErr)
import PrestoDOM.Core (processEvent) as PrestoDom
import Types.App (defaultGlobalState, FlowBT, ScreenType(..))
import Screens.Types(PermissionScreenStage(..))
import AssetsProvider (fetchAssets)
import Timers
import Effect.Uncurried
import Engineering.Helpers.BackTrack (liftFlowBT)

main :: Event -> Effect Unit
main event = do
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ getWindowVariable "__payload" Just Nothing)
  case payload of
    Right payload'  -> do
      mainFiber <- launchAff $ flowRunner defaultGlobalState $ do
          _ <- runExceptT $ runBackT $ updateEventData event
          resp ← runExceptT $ runBackT $ Flow.baseAppFlow payload' true
          case resp of
                Right x → pure unit
                Left err → do
                  _ <- pure $ printLog "printLog error in main is : " err
                  _ <- liftFlow $ main event
                  pure unit
      _ <- launchAff $ flowRunner defaultGlobalState $ do liftFlow $ fetchAssets
      JBridge.storeMainFiberOb mainFiber
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
    _ -> pure unit

onConnectivityEvent :: String -> Effect Unit
onConnectivityEvent triggertype = do
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ getWindowVariable "__payload" Just Nothing)
  case payload of
    Right payload'  -> do
        mainFiber <- launchAff $ flowRunner defaultGlobalState $ do
          _  <- case (runFn2 JBridge.getMainFiber Just Nothing) of
            Nothing -> pure unit
            Just fiber -> liftFlow $ launchAff_ $ killFiber (error "error in killing fiber") fiber
          _ ← runExceptT $ runBackT do
              case triggertype of 
                "LOCATION_DISABLED" -> do
                  modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = LOCATION_DISABLED})
                  Flow.permissionScreenFlow
                "INTERNET_ACTION" -> do
                  modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = INTERNET_ACTION})
                  Flow.permissionScreenFlow
                "REFRESH" -> Flow.baseAppFlow payload' false
                _ -> Flow.baseAppFlow payload' false
          pure unit
        JBridge.storeMainFiberOb mainFiber
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
      _ -> pure unit            

onBundleUpdatedEvent :: FCMBundleUpdate -> Effect Unit
onBundleUpdatedEvent description= do 
  _ <- launchAff $ flowRunner defaultGlobalState $ do
    _ ← runExceptT $ runBackT $ do
      appUpdatedFlow description
    pure unit
  pure unit