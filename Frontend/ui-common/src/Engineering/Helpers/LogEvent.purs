module Engineering.Helpers.LogEvent where

import Effect (Effect)
import Data.Maybe(Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Tracker.Labels (Label(..))
import Foreign (Foreign, readString)
import Control.Monad.Except (runExcept)
import Foreign.Class (class Encode, encode)
import Foreign.Object (Object, empty, insert, lookup)
import Tracker (trackActionObject, trackScreenEnd, trackScreen, trackScreenEvent)
import Tracker.Types (Action(..), Level(..), Screen(..)) as Tracker
import Prelude (Unit, pure, unit, ($), (<$>), (<<<), (/=), (&&),bind)
import Presto.Core.Types.Language.Flow (Flow , doAff)
import JBridge (firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams)
import Log (rootLevelKeyWithRefId)

foreign import isFirebaseEnabledFFI :: forall a. (a -> Maybe a) -> (Maybe a) -> Effect (Maybe a)

isFirebaseEnabled :: Effect Boolean
isFirebaseEnabled = do
    mbIsFireBaseEnabled <- isFirebaseEnabledFFI Just Nothing
    pure $ fromMaybe false mbIsFireBaseEnabled


logEvent' :: Object Foreign -> String -> Effect Unit
logEvent' logField event = do
  isFirebaseEnabled_ <- isFirebaseEnabled
  if isFirebaseEnabled_ then do
    _ <- firebaseLogEvent event
    pure unit
  else do
      let 
        eventObject = insert "event" (encode event) empty
        foreignObject = rootLevelKeyWithRefId logField
      _ <- trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
      pure unit 

logEventWithParams' :: Object Foreign -> String -> String -> String  -> Effect Unit 
logEventWithParams' logField event key value = do 
  isFirebaseEnabled_ <- isFirebaseEnabled 
  if true then do
      _ <-  firebaseLogEventWithParams event key value 
      pure unit 
    else do
      let 
        eventObject = insert "event" (encode event)  $ insert key (encode value) empty 
        foreignObject = rootLevelKeyWithRefId logField
      _ <- trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
      pure unit 

logEventWithTwoParams' :: Object Foreign -> String -> String -> String -> String -> String  -> Effect Unit 
logEventWithTwoParams' logField event param1Key param1Value param2Key param2Value  = do  
  isFirebaseEnabled_ <-  isFirebaseEnabled
  if isFirebaseEnabled_ 
    then do
      _ <- firebaseLogEventWithTwoParams event param1Key param1Value param2Key param2Value
      pure unit 
    else do
      let 
        eventObject = insert "event" (encode event)  $ insert param1Key (encode param1Value) $ insert param2Key (encode param2Value) empty
        foreignObject = rootLevelKeyWithRefId logField
      _ <- trackActionObject Tracker.User Tracker.Info ON_EVENT eventObject foreignObject
      pure unit 