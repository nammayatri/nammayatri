{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import Prelude (Unit, bind, pure, show, unit, ($), (<$>), (<<<), (==), void, discard, identity)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (killFiber, launchAff, launchAff_)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, liftFlow, getWindowVariable, markPerformance, setEventTimestamp)
import AssetsProvider (fetchAssets)
import Flow as Flow
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import PrestoDOM.Core (processEvent) as PrestoDom
import Log
import Presto.Core.Types.API (ErrorResponse(..))
import Presto.Core.Types.Language.Flow (throwErr)
import Foreign (Foreign, MultipleErrors, unsafeToForeign)
import Foreign.Generic (decode)
import Common.Types.App (GlobalPayload, Event, FCMBundleUpdate)
import Types.App (defaultGlobalState)
import Effect.Class (liftEffect)
import Control.Monad.Except (runExcept)
import Data.Maybe (fromMaybe, Maybe(..), maybe)
import Screens.Types (AllocationData)
import Types.ModifyScreenState (modifyScreenState)
import Types.App (FlowBT, ScreenType(..))
import JBridge as JBridge
import Helpers.Utils as Utils
import Effect.Exception (error)
import Data.Function.Uncurried (runFn2, runFn3)
import Screens (ScreenName(..)) as ScreenNames
import Engineering.Helpers.Events as Events
import Data.Array as DA
import Effect.Uncurried (runEffectFn1)
import Screens.Types as ST
import Common.Types.App as Common
import Storage (KeyStore(..), setValueToLocalStore)
import Services.API (GetDriverInfoResp(..))
import DecodeUtil (getFromWindow, removeFromWindow)
import Debug

main :: Event -> Effect Unit
main event = do
  void $ markPerformance "MAIN_START"
  void $ Events.initMeasuringDuration "Flow.mainFlow"
  void $ Events.initMeasuringDuration "mainToHomeScreenDuration"
  let profile = runFn3 getFromWindow "Profile" Nothing Just
      _ = removeFromWindow "Profile"
  let driverInfoResp = case profile of
                        Just driverInfoRespFiber -> 
                          case runExcept (decode driverInfoRespFiber) of
                            Right (driverInfoRes :: GetDriverInfoResp) -> Just (Right driverInfoRes)
                            Left _ -> case runExcept (decode driverInfoRespFiber) of
                              Right (errorRes :: ErrorResponse) ->
                                if (DA.any (\error -> Utils.decodeErrorCode errorRes.response.errorMessage == error) ["VEHICLE_NOT_FOUND", "DRIVER_INFORMATON_NOT_FOUND"]) 
                                  then Just (Left errorRes)
                                  else Nothing
                              Left err -> Nothing
                        Nothing -> Nothing
  let _ = spy "driverInfoResp =>" driverInfoResp
  mainFiber <- launchAff $ flowRunner defaultGlobalState $ do
    liftFlow $ setEventTimestamp "main_purs" 
    _ <- runExceptT $ runBackT $ updateEventData event
    resp ← runExceptT $ runBackT $ Flow.baseAppFlow true Nothing driverInfoResp
    case resp of
      Right _ -> pure $ printLog "printLog " "Success in main"
      Left error -> liftFlow $ main event
  _ <- launchAff $ flowRunner defaultGlobalState $ do liftFlow $ fetchAssets
  void $ markPerformance "MAIN_END"
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
    "onReloadApp" -> main { type: "REFRESH", data : "" }
    _ -> pure unit

onConnectivityEvent :: String -> Effect Unit
onConnectivityEvent triggertype = do
  mainFiber <- launchAff $ flowRunner defaultGlobalState $ do
    _ ← runExceptT $ runBackT $ case triggertype of
      "LOCATION_DISABLED" -> Flow.noInternetScreenFlow triggertype
      "INTERNET_ACTION" -> pure unit
      "REFRESH" -> do
        void $ restorePreviousState
        Flow.baseAppFlow false Nothing Nothing
      "CHECK_NETWORK_TIME" ->  Flow.checkTimeSettings
      _ -> Flow.baseAppFlow false Nothing Nothing
    pure unit
  pure unit

onBundleUpdatedEvent :: FCMBundleUpdate -> Effect Unit
onBundleUpdatedEvent description = do 
  _ <- launchAff $ flowRunner defaultGlobalState $ do
    _ ← runExceptT $ runBackT $ do
      Flow.appUpdatedFlow description ST.NORMAL
    pure unit
  pure unit

onNewIntent :: Event -> Effect Unit
onNewIntent event = do
  _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $
    case event.type of
      "DEEP_VIEW_NEW_INTENT" -> Flow.baseAppFlow false (Just event) Nothing
      "DEEP_VIEW" -> Flow.baseAppFlow true (Just event) Nothing
      "REFERRAL" -> setValueToLocalStore REFERRER_URL event.data
      "REFERRAL_NEW_INTENT" -> do
        setValueToLocalStore REFERRER_URL event.data
        Flow.baseAppFlow true Nothing Nothing
      _ -> Flow.baseAppFlow false Nothing Nothing
  -- required if we need to update Payment page assets in first run
  -- _ <- launchAff $ flowRunner defaultGlobalState $ do liftFlow fetchAssets
  pure unit

updateEventData :: Event -> FlowBT String Unit
updateEventData event = do 
    case event.type of
      "NEW_MESSAGE" -> modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> notificationScreen{ selectedNotification = Just event.data, deepLinkActivated = true })
      "PAYMENT_MODE_MANUAL" -> modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {callScreen = ScreenNames.SUBSCRIPTION_SCREEN})
      _ -> pure unit

restorePreviousState :: FlowBT String Unit
restorePreviousState = do
  let popupType = maybe ST.NO_POPUP_VIEW identity (runFn2 Utils.getPopupType Just Nothing)
  modifyScreenState $ GlobalPropsType (\globalProps -> globalProps { gotoPopupType = popupType})