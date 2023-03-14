module Screens.DriverDetailsScreen.Controller where

import Prelude (class Show, unit, (/=), ($), pure, bind, discard)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import Screens.Types (DriverDetailsScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Language.Strings (getString)
import Language.Types(STR(..))
import Screens.DriverDetailsScreen.ScreenData (ListOptions(..))
import Effect.Class (liftEffect)
import JBridge (renderBase64Image)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Maybe (fromMaybe)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen DRIVER_DETAILS_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen DRIVER_DETAILS_SCREEN)
      trackAppEndScreen appId (getScreen DRIVER_DETAILS_SCREEN)
    CallBackImageUpload str imageName -> trackAppScreenEvent appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "upload_callback_image"
    RenderBase64Image -> trackAppScreenEvent appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "render_base_image"
    UploadFileAction -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "upload_file"
    NoAction -> trackAppScreenEvent appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "no_action"

      
data ScreenOutput = GoBack DriverDetailsScreenState
data Action = NoAction 
              | BackPressed 
              | CallBackImageUpload String String
              | RenderBase64Image
              | AfterRender
              | UploadFileAction

eval :: Action -> DriverDetailsScreenState -> Eval Action ScreenOutput DriverDetailsScreenState
eval BackPressed state = exit (GoBack state)

eval (CallBackImageUpload image imageName) state = if (image /= "") then 
                                            continueWithCmd (state { data { base64Image = image}}) [do pure RenderBase64Image]
                                            else
                                              continue state

eval RenderBase64Image state = continueWithCmd state [do
  _ <- liftEffect $ renderBase64Image state.data.base64Image (getNewIDWithTag "EditProfileImage")
  pure NoAction]
eval AfterRender state = continue state

eval UploadFileAction state = continue state

eval NoAction state = continue state

getTitle :: ListOptions -> String
getTitle listOptions =
  case listOptions of
    DRIVER_NAME_INFO -> (getString NAME)
    DRIVER_MOBILE_INFO -> (getString MOBILE_NUMBER)
    DRIVER_LICENCE_INFO -> (getString DRIVING_LICENSE)

getValue :: ListOptions -> DriverDetailsScreenState -> String
getValue listOptions state =
  case listOptions of
    DRIVER_NAME_INFO -> state.data.driverName
    DRIVER_MOBILE_INFO -> (fromMaybe "" (state.data.driverMobile))
    DRIVER_LICENCE_INFO -> state.data.drivingLicenseNo