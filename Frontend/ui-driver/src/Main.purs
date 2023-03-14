module Main where

import Prelude (Unit, bind, pure, show, unit, ($), (<$>), (<<<), (==))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Engineering.Helpers.Commons (flowRunner, liftFlow, window')
import Flow as Flow
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import PrestoDOM.Core2 (processEvent) as PrestoDom
import Log
import Presto.Core.Types.Language.Flow (throwErr)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Generic (decode)
import Common.Types.App (GlobalPayload)
import Effect.Class (liftEffect)
import Control.Monad.Except (runExcept)
import Data.Maybe (fromMaybe, Maybe(..))
import Screens.Types (AllocationData)

main :: Effect Unit
main = do
  launchAff_ $ flowRunner $ do
    _ <- pure $ printLog "printLog " "in main"
    resp ← runExceptT $ runBackT $ Flow.baseAppFlow
    case resp of
      Right x -> pure $ printLog "printLog " "Success in main"
      Left error -> do
        _ <- pure $ printLog "printLog error in main" error
        liftFlow $ main

mainAllocationPop :: String -> AllocationData -> Effect Unit
mainAllocationPop payload_type entityPayload = do
  _ <- pure $ printLog "entity_payload" entityPayload
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ window' "__payload" Just Nothing)
  case payload of 
    Right payload' -> launchAff_ $ flowRunner $ do
      if(payload_type == "NEW_RIDE_AVAILABLE") then 
        runExceptT $ runBackT $ (Flow.popUpScreenFlow entityPayload)
        else 
          runExceptT $ runBackT $ Flow.homeScreenFlow
      
    Left e -> launchAff_ $ flowRunner $ do
      _ <- pure $ printLog "payload type mismatch " ""
      throwErr $ show e

onEvent :: String -> Effect Unit
onEvent "onBackPressed" = PrestoDom.processEvent "onBackPressedEvent" unit
onEvent _ = pure unit

onConnectivityEvent :: String -> Effect Unit
onConnectivityEvent triggertype = do
  launchAff_ $ flowRunner $ do
    resp ← runExceptT $ runBackT $ Flow.noInternetScreenFlow triggertype
    case resp of 
      Right x -> pure $ printLog "Event" "onConnectivityEvent"
      Left error -> throwErr $ show error