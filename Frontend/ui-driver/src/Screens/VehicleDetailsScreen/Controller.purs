{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.VehicleDetailsScreen.Controller where

import Prelude (class Show, bind, not, pure, unit, ($), (/=), discard)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import Screens.Types (VehicleDetailsScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.SelectVehicleTypeModal as SelectVehicleTypeModal
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.VehicleDetailsScreen.ScreenData (ListOptions(..))
import Effect.Class (liftEffect)
import JBridge (previewImage, uploadFile)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Helpers.Utils (getVehicleType)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen VEHICLE_DETAILS_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen VEHICLE_DETAILS_SCREEN)
      trackAppEndScreen appId (getScreen VEHICLE_DETAILS_SCREEN)
    PrimaryEditTextActionController act -> case act of
      PrimaryEditText.TextChanged id value -> trackAppTextInput appId (getScreen VEHICLE_DETAILS_SCREEN) "registration_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen VEHICLE_DETAILS_SCREEN) "registration_number_text_focus_changed" "primary_edit_text"
    ToggleScreenMode -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "in_screen" "toggle_screen_mode"
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "primary_button" "update_on_click"
        trackAppEndScreen appId (getScreen VEHICLE_DETAILS_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "primary_button" "update_no_action"
    SelectVehicleType -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "in_screen" "select_vehicle_type"
    RemoveImageClick -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "in_screen" "remove_image_click"
    UploadImage -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "in_screen" "upload_image_click"
    PreviewImage -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "in_screen" "preview_image_clicked"
    SelectVehicleTypeModalAction action -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "in_screen" "select_vehicle_type"
    CallBackImageUpload str imageName imagePath -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "in_screen" "call_back_image_upload"
    NoAction -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "in_screen" "no_action"

data ScreenOutput
  = GoBack
  | UpdateVehicleDetails VehicleDetailsScreenState

data Action
  = NoAction
  | PrimaryEditTextActionController PrimaryEditText.Action
  | ToggleScreenMode
  | PrimaryButtonActionController PrimaryButton.Action
  | SelectVehicleType
  | BackPressed
  | SelectVehicleTypeModalAction SelectVehicleTypeModal.Action
  | PreviewImage
  | RemoveImageClick
  | UploadImage
  | CallBackImageUpload String String String
  | AfterRender

eval :: Action -> VehicleDetailsScreenState -> Eval Action ScreenOutput VehicleDetailsScreenState
eval AfterRender state = continue state

eval BackPressed state = exit GoBack

eval ToggleScreenMode state = continue $ state { props { isInEditVehicleDetailsView = not state.props.isInEditVehicleDetailsView } }

eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = exit (UpdateVehicleDetails state)

eval (PrimaryEditTextActionController (PrimaryEditText.TextChanged id value)) state = continue state

eval RemoveImageClick state = continue $ state { data { base64Image = "", imageName = "" }, props { deleteButtonVisibility = false } }

eval (PreviewImage) state =
  continueWithCmd state
    [ do
        _ <- liftEffect $ previewImage state.data.base64Image
        pure NoAction
    ]

eval (UploadImage) state =
  continueWithCmd state
    [ do
        _ <- liftEffect $ uploadFile false
        pure NoAction
    ]

eval (CallBackImageUpload base_64 imageName imagePath) state = do
  if base_64 /= "" then continue $ state { props { deleteButtonVisibility = true }, data { imageName = "image.jpeg", base64Image = base_64 } } else continue state

eval SelectVehicleType state = continue state

eval _ state = continue state

getTitle :: ListOptions -> String
getTitle listOptions = case listOptions of
  RegistrationNumber -> (getString VEHICLE_REGISTRATION_NUMBER)
  VehicleType -> (getString VEHICLE_TYPE)
  VehicleModelName -> (getString VEHICLE_MODEL_NAME)
  VehicleColor -> (getString VEHICLE_COLOUR)
  VehicleRC -> (getString REGISTRATION_CERTIFICATE_IMAGE)

getValue :: ListOptions -> VehicleDetailsScreenState -> String
getValue listOptions state = case listOptions of
  RegistrationNumber -> state.data.vehicleRegNumber
  VehicleType -> (getVehicleType state.data.vehicleType)
  VehicleModelName -> state.data.vehicleModel
  VehicleColor -> state.data.vehicleColor
  VehicleRC -> state.data.imageName
