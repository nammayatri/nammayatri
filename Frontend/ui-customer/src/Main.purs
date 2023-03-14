module Main where

import Common.Types.App (GlobalPayload)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (new)
import Engineering.Helpers.Commons (flowRunner, liftFlow, window')
import Flow as Flow
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Generic (decode)
import JBridge (toggleBtnLoader)
import Log (printLog)
import Prelude (Unit, bind, pure, show, unit, ($), (<$>), (<<<))
import Presto.Core.Types.Language.Flow (throwErr)
import PrestoDOM.Core2 (processEvent) as PrestoDom
import Types.App (defaultGlobalState)

main :: Effect Unit
main = do
  epassRef ← new defaultGlobalState
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ window' "__payload" Just Nothing)
  case payload of
    Right payload'  -> launchAff_ $ flowRunner $ do
      -- _ <- pure $ JBridge._addCertificates (Config.getFingerPrint "")
      resp ← runExceptT $ runBackT $ Flow.baseAppFlow payload'
      case resp of
            Right x → pure unit
            Left err → do
              _ <- pure $ printLog "printLog error in main is : " err
              _ <- liftFlow $ main 
              pure unit
    Left e -> launchAff_ $ flowRunner $ do
      throwErr $ show e

onEvent :: String -> Effect Unit
onEvent "onBackPressed" = do 
  _ <- pure $ toggleBtnLoader "" false
  PrestoDom.processEvent "onBackPressedEvent" unit
onEvent _ = pure unit

onConnectivityEvent :: String -> Effect Unit
onConnectivityEvent triggertype = do
  epassRef ← new defaultGlobalState
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ window' "__payload" Just Nothing)
  case payload of
    Right payload'  -> launchAff_ $ flowRunner $ do
      -- _ <- pure $ JBridge._addCertificates (Config.getFingerPrint "")
      resp ← runExceptT $ runBackT $ Flow.permissionScreenFlow triggertype
      case resp of
            Right x → pure unit
            Left err → pure unit
    Left e -> launchAff_ $ flowRunner $ do
      throwErr $ show e
