module Engineering.Helpers.LogEvent where

import Effect (Effect, foreachE)
import Data.Maybe(Maybe(..), fromMaybe, isJust)
import Data.Array (find)
import Data.Either (Either(..))
import Tracker.Labels (Label(..))
import Foreign (Foreign, readString)
import Control.Monad.Except (runExcept)
import Foreign.Class (class Encode, encode)
import Foreign.Object (Object, empty, insert, lookup)
import Tracker (trackActionObject, trackScreenEnd, trackScreen, trackScreenEvent)
import Tracker.Types (Action(..), Level(..), Screen(..)) as Tracker
import Prelude (Unit, pure, unit, ($), (<$>), (<<<), (/=), (==), (&&), bind, void, when, discard,map)
import Presto.Core.Types.Language.Flow (Flow , doAff)
import JBridge (firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, cleverTapCustomEventWithParams, cleverTapCustomEvent, cleverTapEvent)
import Log (rootLevelKeyWithRefId)
import Effect.Class (liftEffect)
import Common.Types.App (ClevertapEventParams)

foreign import getLogDestination :: Effect (Array String)

isElement :: String -> Array String -> Boolean
isElement logBrand logBrands = isJust $ find (\el -> el == logBrand) logBrands

triggerLog :: String -> Object Foreign -> String -> Effect Unit
triggerLog event logField logDestination = case logDestination of
  "FIREBASE" -> do
    _ <- firebaseLogEvent event
    pure unit
  "JUSPAY" -> do
    let
      eventObject = insert "event" (encode event) empty
      foreignObject = rootLevelKeyWithRefId logField
    _ <- trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
    pure unit
  "CLEVERTAP" -> do
    void $ pure $ cleverTapCustomEvent event
  _ -> do
    pure unit

logEvent :: Object Foreign -> String -> Effect Unit 
logEvent logField event = do 
  logDestinations <- getLogDestination
  void $ foreachE logDestinations (triggerLog event logField)
  pure unit

triggerLogWithParams :: String -> Object Foreign -> String -> String -> String -> Effect Unit 
triggerLogWithParams  event logField key value logDestination = case logDestination of  
  "FIREBASE" -> do 
    _ <- firebaseLogEventWithParams event key value  
    pure unit 
  "JUSPAY" -> do 
    let 
      eventObject = insert "event" (encode event) $ insert key (encode value) empty 
      foreignObject = rootLevelKeyWithRefId logField
    _ <- trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
    pure unit
  "CLEVERTAP" -> do
    void $ pure $ cleverTapCustomEventWithParams event key value
  _ -> do
    pure unit 

logEventWithParams :: Object Foreign -> String -> String -> String  -> Effect Unit 
logEventWithParams logField event key value = do 
  logDestinations <- getLogDestination
  void $ foreachE logDestinations (triggerLogWithParams event logField key value)
  pure unit 

triggerLogWithTwoParams :: String -> Object Foreign -> String -> String -> String -> String -> String -> Effect Unit 
triggerLogWithTwoParams  event logField key1 value1 key2 value2 logDestination = case logDestination of  
  "FIREBASE" -> do 
    _ <- firebaseLogEventWithTwoParams event key1 value1 key2 value2   
    pure unit 
  "JUSPAY" -> do 
    let 
      eventObject = insert "event" (encode event)  $ insert key1 (encode value1) $ insert key2 (encode value2) empty
      foreignObject = rootLevelKeyWithRefId logField
    _ <- trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
    pure unit 
  _ -> do 
    pure unit 

logEventWithTwoParams :: Object Foreign -> String -> String -> String -> String -> String  -> Effect Unit 
logEventWithTwoParams logField event key1 value1 key2 value2  = do 
  logDestinations <- getLogDestination
  void $ foreachE logDestinations (triggerLogWithTwoParams event logField key1 value1 key2 value2)
  pure unit 

triggerLogEventWithMultipleParams :: String -> Object Foreign -> Array ClevertapEventParams -> String -> Effect Unit
triggerLogEventWithMultipleParams event logField arr logDestination = case logDestination of
  "CLEVERTAP" -> do
    void $ pure $ cleverTapEvent event arr
  _ -> pure unit

logEventWithMultipleParams :: Object Foreign -> String -> Array ClevertapEventParams -> Effect Unit
logEventWithMultipleParams logField event arr = do
  logDestinations <- getLogDestination
  void $ foreachE logDestinations (triggerLogEventWithMultipleParams event logField arr)