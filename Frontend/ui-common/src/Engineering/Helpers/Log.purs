module Log where

import Effect (Effect)
import Data.Maybe(Maybe(..))
import Data.Either (Either(..))
import Tracker.Labels (Label(..))
import Foreign (Foreign, readString)
import Control.Monad.Except (runExcept)
import Foreign.Class (class Encode, encode)
import Foreign.Object (Object, empty, insert, lookup)
import Tracker (trackActionObject, trackScreenEnd, trackScreen, trackScreenEvent)
import Tracker.Types (Action(..), Level(..), Screen(..)) as Tracker
import Prelude (Unit, pure, unit, ($), (<$>), (<<<), (/=), (&&),bind)

foreign import log :: String -> Foreign -> Unit

rootLevelKey :: Object Foreign ->  Object Foreign
rootLevelKey logField = do
    let
      bundle_version = lookup "bundle_version" logField
      version_name = lookup "version_name" logField
      platform = lookup "platform" logField
      app_version = lookup "app_version" logField
    insert "bundle_version" (encode bundle_version) $ insert "version_name" (encode version_name) $ insert "platform" (encode platform) $ insert "app_version" (encode app_version) empty

rootLevelKeyWithRefId :: Object Foreign -> Object Foreign
rootLevelKeyWithRefId logField = do
  let
    lookCustomerId = lookup "customer_id" logField
    lookDriverId = lookup "driver_id" logField
    decodeCustomerId = ((runExcept <<< readString) <$> lookCustomerId)
    decodeDriverId = ((runExcept <<< readString) <$> lookDriverId)
    customerId = case decodeCustomerId of
                    Just a -> case a of
                      Right id -> id
                      Left e ->  ""
                    Nothing -> ""
    driverId = case decodeDriverId of
                  Just a -> case a of
                    Right id -> id
                    Left e -> ""
                  Nothing -> ""
  if ((customerId) /= "" && customerId /= "__failed") then insert "customer_id" (encode customerId) $ rootLevelKey logField
    else if (driverId /= "" && driverId /= "__failed") then insert "driver_id" (encode driverId) $ rootLevelKey logField
      else rootLevelKey logField

trackAppBackPress :: Object Foreign -> String -> Effect Unit
trackAppBackPress logField str = do
  let
    x = insert "screen_name" (encode str) $ insert "click_name" (encode "back_press") empty
    y = rootLevelKeyWithRefId logField
  _ <- trackActionObject Tracker.User Tracker.Info BACKPRESS x y
  pure unit

trackAppActionClick :: Object Foreign -> String -> String -> String -> Effect Unit
trackAppActionClick logField str clickCategory value = do
  let
    x = insert "screen_name" (encode str) $ insert "click_category" (encode clickCategory) $ insert "click_name" (encode value) empty
    y = rootLevelKeyWithRefId logField
  _ <- trackActionObject Tracker.User Tracker.Info ON_CLICK x y
  pure unit

trackAppTextInput :: Object Foreign -> String -> String -> String -> Effect Unit
trackAppTextInput logField str clickName value = do
  let
    x = insert "screen_name" (encode str) $ insert "action_name" (encode clickName) $ insert "field_details" (encode { "field_name": value }) empty
    y = rootLevelKeyWithRefId logField
  _ <- trackActionObject Tracker.User Tracker.Info FIELD_FOCUSSED x y
  pure unit

trackAppScreenRender :: Object Foreign -> String -> String -> Effect Unit
trackAppScreenRender logField presentation screen = do
  let x = rootLevelKeyWithRefId logField
  _ <- trackScreen Tracker.Screen Tracker.Info SCREEN_RENDERED presentation screen x
  pure unit

trackAppEndScreen :: Object Foreign -> String -> Effect Unit
trackAppEndScreen logField screen = do
  let x = rootLevelKeyWithRefId logField
  _ <- trackScreenEnd screen x
  pure unit

trackAppScreenEvent :: Object Foreign -> String -> String -> String -> Effect Unit
trackAppScreenEvent logField screen_name event_category event_name = do
  let
    x = insert "screen_name" (encode screen_name) $ insert "event_category" (encode event_category) $ insert "event_name" (encode event_name) empty
    y = rootLevelKeyWithRefId logField
  _ <- trackActionObject Tracker.User Tracker.Info ON_EVENT x y
  pure unit

printLog :: forall a. Encode a => String -> a -> Unit
printLog a b = log a (encode b)
