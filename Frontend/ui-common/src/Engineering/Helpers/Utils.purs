module Engineering.Helpers.Utils where

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
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Foreign.Generic (decode, encode)
import MerchantConfig.Types (AppConfig)
import MerchantConfig.DefaultConfig as DefaultConfig
import Types.App (FlowBT)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))


foreign import toggleLoaderIOS :: EffectFn1 Boolean Unit

foreign import loaderTextIOS :: EffectFn2 String String Unit

foreign import getMerchantConfig :: forall a. (a -> Maybe a) -> (Maybe a) -> Effect (Maybe a)

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
      _ <- modifyState (\(GlobalState state) -> GlobalState state{loaderOverlay{data{title = mainTxt, subTitle = subTxt}}})
      pure unit

getSeparatorFactor :: Int
getSeparatorFactor = 8

defaultSeparatorCount :: Int
defaultSeparatorCount = 4

showAndHideLoader :: Number -> String -> String -> GlobalState -> Effect Unit
showAndHideLoader delayInMs title description state = do
  _ <- launchAff $ flowRunner state $ do
    _ <- loaderText title description
    _ <- toggleLoader true
    _ <- delay $ Milliseconds delayInMs
    _ <- toggleLoader false
    pure unit
  pure unit

mobileNumberValidator :: String -> String -> String -> Boolean 
mobileNumberValidator country countryShortCode mobileNumber = 
  case countryShortCode of 
    "IN" -> (length mobileNumber == 10) && 
            case (charAt 0 mobileNumber) of
              Just a -> if a=='0' || a=='1' || a=='2' || a=='3' || a=='4' then false
                          else if a=='5' then
                              if mobileNumber=="5000500050" then true else false 
                                  else true 
              Nothing -> true 
    "FR" -> (length mobileNumber == 9) && 
            case (charAt 0 mobileNumber) of 
              Just a -> a == '6' || a == '7'
              Nothing -> false
    "BD" -> (length mobileNumber == 10) && 
            case (charAt 0 mobileNumber) of 
              Just a -> a == '1'
              Nothing -> false
    _ -> false 

mobileNumberMaxLength :: String -> Int 
mobileNumberMaxLength countryShortCode = 
  case countryShortCode of 
    "IN" -> 10
    "FR" -> 9 
    "BD" -> 10
    _ -> 0

getAppConfig :: FlowBT String AppConfig
getAppConfig = liftFlowBT $ getAppConfigImpl

getAppConfigImpl :: Effect AppConfig
getAppConfigImpl  = do
  config' <- getConfig
  pure $
    case config' of
      Just config -> do
        case runExcept (decode (encode config )) of
            Right (obj :: AppConfig) -> config
            Left err ->DefaultConfig.config
      Nothing -> do
            DefaultConfig.config

getConfig :: forall  a. Effect (Maybe a)
getConfig = getMerchantConfig Just Nothing
