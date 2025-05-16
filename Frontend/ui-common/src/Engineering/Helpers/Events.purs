module Engineering.Helpers.Events where

import Prelude
import Data.Maybe
import Data.Function.Uncurried (Fn2(..))
import Control.Monad.Except.Trans (lift)
import Engineering.Helpers.Commons (liftFlow)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Presto.Core.Types.Language.Flow (Flow)
import Effect (Effect)
import Debug
import Common.Types.App (FlowBT)
import Data.Array (length, take, reverse, drop)
import Helpers.API as HelpersAPI
import Services.API as APITypes
import Presto.Core.Types.API (ErrorResponse)
import Data.Either (Either(..))
import JBridge as JB
import DecodeUtil (decodeForeignAny, parseJSON, stringifyJSON)
import Data.Traversable (traverse, for_)
import Screens.Types as ST
import Engineering.Helpers.Commons as EHC
import Storage (getValueToLocalStore, KeyStore(..))
import MerchantConfig.Utils (getMerchant)
import Effect.Aff (launchAff, Milliseconds(..))
import Common.Types.App as CT
import RemoteConfig as RC
import Helpers.Pooling(delay)
import Log (printLog)

------------------------------------------------------------------------------
------------------------- Namma Yatri Event Pipeline -------------------------
------------------------------------------------------------------------------
foreign import initMeasuringDuration :: String -> Effect Unit
foreign import endMeasuringDuration :: String -> Effect Unit
foreign import addEventData :: String -> String -> Effect Unit
foreign import getEvents :: Effect String

measureDuration :: forall a. String -> Effect a -> Effect a
measureDuration name action = do
  void $ initMeasuringDuration name
  result <- action
  void $ endMeasuringDuration name
  pure result

measureDurationFlow :: forall e st. String -> Flow e st -> Flow e st
measureDurationFlow name action = do  
  void $ liftFlow $ initMeasuringDuration name
  result <- action  
  void $ liftFlow $ endMeasuringDuration name
  pure result

measureDurationFlowBT :: forall e st s. String -> FlowBT e st s -> FlowBT e st s
measureDurationFlowBT name action = do  
  liftFlowBT $ initMeasuringDuration name
  result <- action
  liftFlowBT $ endMeasuringDuration name
  pure result


getEventFromStorage :: Unit -> Array Event
getEventFromStorage _ = do
  let stringifiedEvents = JB.getKeyInSharedPrefKeys "EVENT_STORAGE"
  decodeForeignAny (parseJSON stringifiedEvents) $ []

setEventToStorage :: Event -> Effect Unit
setEventToStorage event = do
  let events = (getEventFromStorage unit) <> [event]
  let _ = JB.setKeyInSharedPrefKeys "EVENT_STORAGE" $ stringifyJSON events
  pure unit

clearEventStorage :: Unit -> Unit
clearEventStorage _ = do
  let _ = JB.removeKeysInSharedPrefs "EVENT_STORAGE"
  unit

addEvent :: Event -> Effect Unit
addEvent event = do
  void $ launchAff $ EHC.flowRunner unit $ liftFlow $ setEventToStorage updatedEvent
  where 
    updatedEvent = -- Added this to call these functions on a new thread
      event { 
        clientType = show $ getMerchant CT.FunctionCall,
        sessionId = JB.getKeyInSharedPrefKeys "SESSION_ID",
        userId = JB.getKeyInSharedPrefKeys "DRIVER_ID",
        timestamp = EHC.getCurrentUTC "",
        vehicleType = JB.getKeyInSharedPrefKeys "VEHICLE_CATEGORY",
        cityId = JB.getKeyInSharedPrefKeys "DRIVER_LOCATION"
      }

pushEvent :: forall st. Int -> Flow st Unit
pushEvent noOfChunk = do
  let events = getEventFromStorage unit
  if ((length events) > 0) then do
    -- create chunks of events
    let eventChunks = chunkEvents events noOfChunk
    -- Call the API for each chunk of events
    for_ eventChunks \chunk -> do
      (resp :: (Either ErrorResponse APITypes.SDKEventsReq)) <- HelpersAPI.callApi $ (APITypes.SDKEventsReq { event: "", events : mkEventPayload events })
      pure unit
    let _ = clearEventStorage unit
    pure unit
  else pure unit

chunkEvents :: Array Event -> Int -> Array (Array Event)
chunkEvents events chunkSize = go events []
  where
    go :: Array Event -> Array (Array Event) -> Array (Array Event)
    go [] acc = reverse acc
    go es acc = go (drop chunkSize es) (acc <> [take chunkSize es])

nullToMaybe :: String -> Maybe String
nullToMaybe "" = Nothing
nullToMaybe "__failed" = Nothing
nullToMaybe x = Just x

replaceFailedWithEmpty :: String -> String
replaceFailedWithEmpty "__failed" = ""
replaceFailedWithEmpty x = x 

type Event = { 
  name :: String, 
  module :: String,
  clientType :: String,
  sessionId :: String,
  payload :: String,
  source :: String,
  userId :: String,
  timestamp :: String,
  vehicleType :: String,
  cityId :: String
  }

defaultEventObject :: String -> Event
defaultEventObject eventName = 
  { module : "",
    name : eventName,
    payload : "",
    source : "",
    clientType : "",
    sessionId : "",
    userId : "",
    timestamp : "",
    vehicleType : "",
    cityId : ""
  }

mkEventPayload :: Array Event -> Array APITypes.EventsPayload
mkEventPayload eventArray = map (\event -> (APITypes.EventsPayload
  { module : nullToMaybe event.module,
    eventName : replaceFailedWithEmpty event.name,
    clientType : replaceFailedWithEmpty event.clientType,
    sessionId : replaceFailedWithEmpty event.sessionId,
    payload : nullToMaybe event.payload,
    source : replaceFailedWithEmpty event.source,
    userId : replaceFailedWithEmpty event.userId,
    timestamp : replaceFailedWithEmpty event.timestamp,
    vehicleType : nullToMaybe event.vehicleType,
    cityId : nullToMaybe event.cityId
    })) eventArray

runLogTracking :: forall st. Flow st Unit
runLogTracking = do
  let eventsConfig = RC.eventsConfig "events_config"
  void $ delay $ Milliseconds eventsConfig.loggingIntervalInMs
  let _ = printLog "Logging Onboarding Events for Customer" ""
  void $ pushEvent eventsConfig.pushEventChunkSize
  runLogTracking
