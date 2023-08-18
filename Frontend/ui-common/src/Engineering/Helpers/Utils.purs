{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}module Engineering.Helpers.Utils where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect(..))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1(..), EffectFn2(..), EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, liftFlow, os)
import Engineering.Helpers.Commons (os)
import Foreign.Generic (Foreign, decode, encode)
import LoaderOverlay.Handler as UI
import Log (printLog)
import MerchantConfig.DefaultConfig as DefaultConfig
import MerchantConfig.Types (AppConfig)
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, modifyState)
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM.Core (terminateUI)
import Types.App (FlowBT)
import Types.App (GlobalState(..))

foreign import toggleLoaderIOS :: EffectFn1 Boolean Unit

foreign import loaderTextIOS :: EffectFn2 String String Unit

foreign import getFromWindow :: EffectFn1 String Foreign

toggleLoader :: Boolean -> Flow GlobalState Unit
toggleLoader flag = do
  if os == "IOS" then do
    _ <- liftFlow $ runEffectFn1 toggleLoaderIOS flag
    pure unit
  else if flag then do
    state <- getState
    _ <- liftFlow $ launchAff $ flowRunner state UI.loaderScreen
    pure unit
  else do
    doAff $ liftEffect $ terminateUI $ Just "LoaderOverlay"

loaderText :: String -> String -> Flow GlobalState Unit
loaderText mainTxt subTxt = do
  if os == "IOS" then do
    _ <- liftFlow $ runEffectFn2 loaderTextIOS mainTxt subTxt
    pure unit
  else do
    _ <- modifyState (\(GlobalState state) -> GlobalState state { loaderOverlay { data { title = mainTxt, subTitle = subTxt } } })
    pure unit

getSeparatorFactor :: Int
getSeparatorFactor = 8

defaultSeparatorCount :: Int
defaultSeparatorCount = 4

showAndHideLoader :: Number -> String -> String -> GlobalState -> Effect Unit
showAndHideLoader delayInMs title description state = do
  _ <-
    launchAff $ flowRunner state
      $ do
          _ <- loaderText title description
          _ <- toggleLoader true
          _ <- delay $ Milliseconds delayInMs
          _ <- toggleLoader false
          pure unit
  pure unit

mobileNumberValidator :: String -> String -> String -> Boolean
mobileNumberValidator country countryShortCode mobileNumber = case countryShortCode of
  "IN" ->
    (length mobileNumber == 10)
      && case (charAt 0 mobileNumber) of
          Just a ->
            if a == '0' || a == '1' || a == '2' || a == '3' || a == '4' then
              false
            else if a == '5' then
              if mobileNumber == "5000500050" then true else false
            else
              true
          Nothing -> true
  "FR" ->
    (length mobileNumber == 9)
      && case (charAt 0 mobileNumber) of
          Just a -> a == '6' || a == '7'
          Nothing -> false
  "BD" ->
    (length mobileNumber == 10)
      && case (charAt 0 mobileNumber) of
          Just a -> a == '1'
          Nothing -> false
  _ -> false

mobileNumberMaxLength :: String -> Int
mobileNumberMaxLength countryShortCode = case countryShortCode of
  "IN" -> 10
  "FR" -> 9
  "BD" -> 10
  _ -> 0

getAppConfig :: String -> FlowBT String AppConfig
getAppConfig = liftFlowBT <<< runEffectFn1 getAppConfigImpl

getAppConfigImpl :: EffectFn1 String AppConfig
getAppConfigImpl =
  mkEffectFn1 \key -> do
    config <- runEffectFn1 getFromWindow key
    case runExcept (decode config) of
      Right obj -> pure obj
      Left err -> do
        _ <- pure $ printLog ("Not able to decode config" <> show err) config
        pure $ DefaultConfig.config
