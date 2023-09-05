{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}module Engineering.Helpers.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Presto.Core.Types.Language.Flow (delay)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, liftFlow, os)
import LoaderOverlay.Handler as UI
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, modifyState)
import PrestoDOM.Core (terminateUI)
import Types.App (GlobalState(..))
import Debug (spy)
import Engineering.Helpers.Commons (os)
import Effect (Effect (..))
import Effect.Uncurried (EffectFn2(..), runEffectFn2, EffectFn1(..), runEffectFn1)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, Fn5, Fn6)
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Foreign.Generic (decode, encode)
import MerchantConfig.Types (AppConfig)
import MerchantConfig.DefaultConfig as DefaultConfig
import Types.App (FlowBT)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Common.Types.App (MobileNumberValidatorResp(..), SqlSchema(..))
import Foreign (Foreign)
import Effect.Uncurried (mkEffectFn1)
import Log (printLog)


foreign import toggleLoaderIOS :: EffectFn1 Boolean Unit

foreign import loaderTextIOS :: EffectFn2 String String Unit

foreign import getFromWindow :: EffectFn1 String Foreign

foreign import deleteDb :: Fn1 String Unit
foreign import createTable :: Fn3 String String (Array (SqlSchema)) Unit
foreign import deleteTable :: Fn2 String String Unit
foreign import addToSqlite :: forall st. Fn3 String String st Unit
foreign import readFromSqlite :: forall st. Fn6 String String String (Array String) (st -> Maybe st) (Maybe st) (Maybe st)
foreign import deleteFromSqlite :: Fn4 String String String (Array String) Boolean
foreign import updateInSqlite :: forall st. Fn5 String String String (Array String) st Int

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

mobileNumberValidator :: String -> String -> String -> MobileNumberValidatorResp 
mobileNumberValidator country countryShortCode mobileNumber = 
  let len = length mobileNumber
      maxLen = mobileNumberMaxLength countryShortCode
  in if len <=  maxLen then 
      case countryShortCode of 
        "IN" -> case (charAt 0 mobileNumber) of
                  Just a -> if a=='0' || a=='1' || a=='2' || a=='3' || a=='4' then Invalid
                            else if a=='5' then if mobileNumber=="5000500050" then Valid else Invalid
                                 else if len == maxLen then Valid else ValidPrefix 
                  Nothing -> ValidPrefix 
        "FR" -> case (charAt 0 mobileNumber) of 
                  Just a -> if a == '6' || a == '7' then if len == maxLen then Valid else ValidPrefix
                            else Invalid 
                  Nothing -> ValidPrefix
        "BD" -> case (charAt 0 mobileNumber) of 
                  Just a -> if a == '1' then if len == maxLen then Valid else ValidPrefix 
                            else Invalid
                  Nothing -> ValidPrefix
        _ -> Invalid
      else MaxLengthExceeded

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
