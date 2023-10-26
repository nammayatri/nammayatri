{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import Prelude (Unit, bind, pure, show, unit, ($), (<$>), (<<<), (==), void, discard)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (killFiber, launchAff, launchAff_)
import Engineering.Helpers.Commons (flowRunner, liftFlow, getWindowVariable)
import Flow as Flow
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import PrestoDOM.Core (processEvent) as PrestoDom
import Log
import Presto.Core.Types.Language.Flow (throwErr)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Generic (decode)
import Common.Types.App (GlobalPayload, Event, FCMBundleUpdate)
import Types.App (defaultGlobalState)
import Effect.Class (liftEffect)
import Control.Monad.Except (runExcept)
import Data.Maybe (fromMaybe, Maybe(..))
import Screens.Types (AllocationData)
import Types.ModifyScreenState (modifyScreenState)
import Types.App (FlowBT, ScreenType(..))
import JBridge as JBridge
import Helpers.Utils as Utils
import Effect.Exception (error)
import Data.Function.Uncurried (runFn2)
import Screens (ScreenName(..)) as ScreenNames
import Data.Array as DA
import Effect.Uncurried (runEffectFn1)

main :: Event -> Effect Unit
main event = do
  mainFiber <- launchAff $ flowRunner defaultGlobalState $ do
    _ <- runExceptT $ runBackT $ updateEventData event
    resp ← runExceptT $ runBackT $ Flow.baseAppFlow true Nothing
    case resp of
      Right _ -> pure $ printLog "printLog " "Success in main"
      Left error -> liftFlow $ main event
  _ <- launchAff $ flowRunner defaultGlobalState $ do liftFlow $ Utils.fetchFiles
  pure unit

mainAllocationPop :: String -> AllocationData -> Effect Unit
mainAllocationPop payload_type entityPayload = do
  _ <- pure $ printLog "entity_payload" entityPayload
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ getWindowVariable "__payload" Just Nothing)
  case payload of
    Right _ -> void $ launchAff $ flowRunner defaultGlobalState $ do
      if(payload_type == "NEW_RIDE_AVAILABLE") then
        runExceptT $ runBackT $ (Flow.popUpScreenFlow entityPayload)
        else
          runExceptT $ runBackT $ Flow.homeScreenFlow

    Left e -> void $ launchAff $ flowRunner defaultGlobalState $ do
      _ <- pure $ printLog "payload type mismatch " ""
      throwErr $ show e

-- TODO :: use this case when on click of notification we want to go to alert section from app itself
-- alertNotification :: String -> Effect Unit
-- alertNotification id = do
--   void $ launchAff $ flowRunner $ do
--     resp ← runExceptT $ runBackT $ Flow.alertNotification id
--     case resp of
--       Right x -> pure $ printLog "Event" "alertNotification"
--       Left error -> throwErr $ show error

onEvent :: String -> Effect Unit
onEvent event = do
  _ <- pure $ JBridge.toggleBtnLoader "" false
  case event of 
    "onBackPressed" -> do
      PrestoDom.processEvent "onBackPressedEvent" unit
    _ -> pure unit

onConnectivityEvent :: String -> Effect Unit
onConnectivityEvent triggertype = do
  mainFiber <- launchAff $ flowRunner defaultGlobalState $ do
    _  <- case (runFn2 JBridge.getMainFiber Just Nothing) of
      Nothing -> pure unit
      Just fiber -> liftFlow $ launchAff_ $ killFiber (error "error in killing fiber") fiber
    _ ← runExceptT $ runBackT $ case triggertype of
      "LOCATION_DISABLED" -> Flow.noInternetScreenFlow triggertype
      "INTERNET_ACTION" -> Flow.noInternetScreenFlow triggertype
      "REFRESH" -> Flow.baseAppFlow false Nothing
      "CHECK_NETWORK_TIME" ->  Flow.checkTimeSettings
      _ -> Flow.baseAppFlow false Nothing
    pure unit
  JBridge.storeMainFiberOb mainFiber
  pure unit

onBundleUpdatedEvent :: FCMBundleUpdate -> Effect Unit
onBundleUpdatedEvent description= do 
  _ <- launchAff $ flowRunner defaultGlobalState $ do
    _ ← runExceptT $ runBackT $ do
      Flow.appUpdatedFlow description
    pure unit
  pure unit

onNewIntent :: Event -> Effect Unit
onNewIntent event = do
  mainFiber <- launchAff $ flowRunner defaultGlobalState $ do
    _ ← runExceptT $ runBackT $ case event.type of
      "DEEP_VIEW_NEW_INTENT" -> Flow.baseAppFlow false (Just event)
      "DEEP_VIEW" -> Flow.baseAppFlow true (Just event)
      _ -> Flow.baseAppFlow false Nothing
    pure unit
  _ <- launchAff $ flowRunner defaultGlobalState $ do liftFlow $ Utils.fetchFiles
  JBridge.storeMainFiberOb mainFiber
  pure unit

updateEventData :: Event -> FlowBT String Unit
updateEventData event = do 
    case event.type of
      "NEW_MESSAGE" -> modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> notificationScreen{ selectedNotification = Just event.data, deepLinkActivated = true })
      "PAYMENT_MODE_MANUAL" -> modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {callScreen = ScreenNames.SUBSCRIPTION_SCREEN})
      _ -> pure unit
