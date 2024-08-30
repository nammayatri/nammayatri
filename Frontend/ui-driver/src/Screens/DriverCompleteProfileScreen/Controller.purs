module Screens.DriverCompleteProfileScreen.Controller where

import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Data.Array (filter, length, (!!), snoc, deleteAt, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Log (trackAppScreenRender)
import Prelude (class Show, map, pure, show, unit, (<>), (==), (&&), not, ($), (>), (/=), (+), (<), (-), discard, void)
import PrestoDOM (Eval, update, continue, exit, updateAndExit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (DriverCompleteProfileScreenState)
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Helpers.Utils (getVehicleVariantImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Array as DA
import Components.PrimaryButton.Controller as PrimaryButton
import Engineering.Helpers.Utils as EHU
import Components.Calendar.Controller as CalendarController
import Common.Resources.Constants (maxImageUploadInIssueReporting)
import Effect.Uncurried (runEffectFn3, runEffectFn1, runEffectFn4)
import Services.EndPoints as EndPoint
import Components.AddImagesModel.Controller as AddImagesModel
import Components.ViewImageModel as ViewImageModel
import Engineering.Helpers.Commons (getNewIDWithTag, getCurrentUTC)
import Effect.Class (liftEffect)
import Data.TraversableWithIndex (forWithIndex)
import JBridge (addMediaFile, clearFocus, generatePDF, hideKeyboardOnNavigation, lottieAnimationConfig, removeMediaPlayer, renderBase64ImageFile, saveAudioFile, scrollToEnd, startAudioRecording, startLottieProcess, stopAudioRecording, toast, uploadFile, uploadMultiPartData, openUrlInApp)
import Data.Int(toNumber, fromString)
import Components.InputTextView as InputTextView
import Components.PrimaryButton.Controller as PrimaryButtonController
import Services.API (DriverProfileDataRes(..))
import Engineering.Helpers.Commons as EHC

data Action = OnClickPledge String Boolean 
            | OnClickVehicalOffer String Boolean 
            | OnClickLanguages String Boolean 
            | OnClickWhyNy String Boolean 
            | OnClickDone PrimaryButton.Action 
            | TextChanged String String 
            | ShowCalendarPopup 
            | CalendarAC CalendarController.Action 
            | DatePicker String String Int Int Int 
            | CallBackImageUpload String String String 
            | ImageUploadCallback  String String String 
            | UploadMultiPartDataCallback  String String 
            | AddImagesModelAction AddImagesModel.Action 
            | ViewImageModelAction ViewImageModel.Action 
            | OnClickUpload 
            | OnClickDelete Int
            | GoBack 
            | NoAction
            | OnDateSelect Int Int
            | Done
            | InputTextAC InputTextView.Action 
            | ProfileDataAPIResponseAction DriverProfileDataRes

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data ScreenOutput = SubmitRes DriverCompleteProfileScreenState | GoToProfile DriverCompleteProfileScreenState

eval :: Action -> DriverCompleteProfileScreenState -> Eval Action ScreenOutput DriverCompleteProfileScreenState

eval (OnClickPledge title isSelected) state = do
  if title == "Other" then do
    if isSelected then continue state { data { inputTextState { others { pledge = "" } } } } else continue state { data { inputTextState { component = "pledge" } }, props { showInputTextView = true } }
  else continue state {data {pledge = if isSelected == true then DA.filter (\item -> (item /= title) ) state.data.pledge else state.data.pledge <> [title] }}

eval (OnClickVehicalOffer title isSelected) state = continue state {data {vehicalOffer = if isSelected == true then DA.filter (\item -> (item /= title) ) state.data.vehicalOffer else state.data.vehicalOffer <> [title] }}

eval (OnClickLanguages title isSelected) state = continue state {data {languages = if isSelected == true then DA.filter (\item -> (item /= title) ) state.data.languages else state.data.languages <> [title] }}

eval (OnClickWhyNy title isSelected) state = do 
  if title == "Other" then do 
    if isSelected then continue state { data { inputTextState { others { whyNy = "" } } } } else continue state {data { inputTextState { component = "WhyNy" } }, props { showInputTextView = true }}
  else continue state {data {whyNy = if isSelected == true then DA.filter (\item -> (item /= title) ) state.data.whyNy else state.data.whyNy <> [title] }}

eval (OnClickDone PrimaryButton.OnClick) state = do
  let newState = state{data{pledge = if state.data.inputTextState.others.pledge == "" then state.data.pledge else state.data.pledge <> [state.data.inputTextState.others.pledge], whyNy = if state.data.inputTextState.others.whyNy == "" then state.data.whyNy else state.data.whyNy <> [state.data.inputTextState.others.whyNy]}}
  updateAndExit newState $ SubmitRes newState

eval (TextChanged id value) state = continue state {data {homeTown = Just value}}

eval GoBack state = exit $ GoToProfile state

eval (OnDateSelect idx item) state = continue state{ data {datePickerState { activeIndex = idx }, drivingSince = Just item}}

eval  (DatePicker _ resp year month date ) state = do
  case resp of 
    "SELECTED" -> do 
      let selectedDateString = year
      
      continue state { data { drivingSince = Just selectedDateString } }
    _ -> continue state

eval ShowCalendarPopup state = do
  let
    res = EHU.initializeCalendar true
  continue state { data { calendarState { weeks = res.weeks, calendarPopup = true, selectedTimeSpan = res.selectedTimeSpan, startDate = res.startDate, endDate = Nothing } } }
    
eval (InputTextAC (InputTextView.FeedbackChanged value)) state = continue state { data { inputTextState { feedback = value } } }

eval (InputTextAC (InputTextView.PrimaryButtonAC PrimaryButtonController.OnClick)) state = 
  if state.data.inputTextState.component == "pledge" then
    continue state { data { inputTextState { others { pledge = state.data.inputTextState.feedback }, feedback = "", component = "" } } , props { showInputTextView = false } }
  else continue state { data { inputTextState { others { whyNy = state.data.inputTextState.feedback }, feedback = "", component = "" } } , props { showInputTextView = false } }

eval (InputTextAC (InputTextView.PrimaryButtonAC PrimaryButtonController.NoAction)) state = continue state

eval (InputTextAC (InputTextView.CancelButtonAC PrimaryButtonController.OnClick)) state = continue state { data { inputTextState { feedback = "", component = "" } }, props { showInputTextView = false } }

eval (InputTextAC (InputTextView.BackPress)) state = continue state { data { inputTextState { feedback = "", component = "" } }, props { showInputTextView = false } }

eval (ProfileDataAPIResponseAction res) state = do 
  let DriverProfileDataRes resp = res 
      pledgeOthers = (DA.filter (\item -> (item /= "Safe Journey" && item /= "Clean Car" && item /= "On-time Pick up" && item /= "Regular Maintenance") )) resp.pledges
      whyNyOthers = (DA.filter (\item -> (item /= "Buy new home" && item /= "Kid’s Education" && item /= "Buy new vehicle") )) resp.aspirations
  continue state{data{
    pledge = resp.pledges, 
    whyNy = resp.aspirations, 
    drivingSince = resp.drivingSince, 
    vehicalOffer = resp.vehicleTags, 
    inputTextState { others {pledge = if DA.length pledgeOthers == 0 then "" else fromMaybe "" $ DA.head pledgeOthers, whyNy = if DA.length whyNyOthers == 0 then "" else fromMaybe "" $ DA.head whyNyOthers}},
    homeTown = resp.hometown, 
    datePickerState {activeIndex = if resp.drivingSince == Nothing then 0 else 69 - ((fromMaybe 0 $ fromString (EHC.convertUTCtoISC (EHC.getCurrentUTC "") "YYYY")) - fromMaybe 0 resp.drivingSince)},
    addImagesState{ images = DA.mapWithIndex(\idx item -> {image : item, imageName :( "Image" <> show idx)})resp.otherImages, imageMediaIds = resp.otherImageIds, stateChanged = true}, 
    uploadedImagesIds = resp.otherImageIds, addedImages = DA.mapWithIndex(\idx item -> {image : item, imageName : ("Image" <> show idx)})resp.otherImages}}

eval (ImageUploadCallback image imageName imagePath) state = do
  let images' = if length state.data.addImagesState.imageMediaIds == maxImageUploadInIssueReporting
                then do
                  pure $ toast $ getString MAX_IMAGES
                  state.data.addImagesState.images
                else
                  snoc state.data.addImagesState.images { image, imageName }
  continueWithCmd state { data { addImagesState { images = images',  isLoading = true } } } [do
    void $ runEffectFn3 uploadMultiPartData imagePath (EndPoint.uploadFile "") "Image"
    pure NoAction
  ]

eval (UploadMultiPartDataCallback fileType fileId) state = do
  let uploadedImagesIds' = if length state.data.addImagesState.imageMediaIds == maxImageUploadInIssueReporting
                            then do
                              state.data.addImagesState.imageMediaIds
                            else
                              snoc state.data.addImagesState.imageMediaIds fileId
  continue state { data { addImagesState {isLoading = false, stateChanged = true, imageMediaIds = uploadedImagesIds' } } }

eval (OnClickUpload) state = continue state {props {showImageModel = not state.props.showImageModel}}

---------------------------------------------------- Add Image Model ----------------------------------------------------

eval (AddImagesModelAction (AddImagesModel.AddImage)) state =
  continueWithCmd state [do
    void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = "primary_button_loader.json", lottieId = (getNewIDWithTag "add_images_model_done_button"), scaleType = "CENTER_CROP" }
    void $ liftEffect $ uploadFile true
    pure NoAction
  ]

eval (AddImagesModelAction (AddImagesModel.OnClickView image imageName)) state = continue state { data  { viewImageState { image = image, imageName = Just imageName } }, props { showViewImageModel = true } }

eval (AddImagesModelAction (AddImagesModel.OnClickDone PrimaryButton.OnClick)) state = do
  continue state { data  { uploadedImagesIds = state.data.addImagesState.imageMediaIds, addedImages = state.data.addImagesState.images }
                 , props { showImageModel = false } }

eval (AddImagesModelAction (AddImagesModel.OnClickCancel)) state = do
  continue state { props { showImageModel = false }
                 , data  { addImagesState { imageMediaIds = state.data.uploadedImagesIds, images = state.data.addedImages } } }

eval (AddImagesModelAction (AddImagesModel.OnClickDelete index)) state = do
  let images'   = fromMaybe state.data.addImagesState.images        $ deleteAt index state.data.addImagesState.images
      imageIds' = fromMaybe state.data.addImagesState.imageMediaIds $ deleteAt index state.data.addImagesState.imageMediaIds
  continueWithCmd state { data { addImagesState { images = images', stateChanged = not (imageIds' == state.data.uploadedImagesIds), imageMediaIds = imageIds' } } } [do -- continueWithCmd state { data { uploadedImagesIds = imageIds',  addedImages = images',  addImagesState { images = images', stateChanged = not (imageIds' == state.data.uploadedImagesIds), imageMediaIds = imageIds' } }, props  } [do
    void $ forWithIndex images' \i x -> do
      void $ runEffectFn4 renderBase64ImageFile x.image (getNewIDWithTag "add_image_component_image" <> (show i)) false "CENTER_CROP"
      pure NoAction
    pure NoAction
  ]

eval (AddImagesModelAction AddImagesModel.BackPressed) state = do
  continueWithCmd state [do
    pure (AddImagesModelAction (AddImagesModel.OnClickCancel))
  ]

---------------------------------------------------- View Image Model ----------------------------------------------------
eval (ViewImageModelAction (ViewImageModel.BackPressed)) state = do
  if state.props.showViewImageModel
  then
    continue state { data  { viewImageState { image = "", imageName = Nothing } }
                   , props { showViewImageModel = false } }
  else
    continueWithCmd state [do
      if state.props.showImageModel
      then pure $ (AddImagesModelAction AddImagesModel.BackPressed)
      else pure $ NoAction
    ]

eval (OnClickDelete index) state = do
  let images'   = fromMaybe state.data.addImagesState.images        $ deleteAt index state.data.addImagesState.images
      imageIds' = fromMaybe state.data.addImagesState.imageMediaIds $ deleteAt index state.data.addImagesState.imageMediaIds
  continueWithCmd state { data { addImagesState { images = images', stateChanged = not (imageIds' == state.data.uploadedImagesIds), imageMediaIds = imageIds' } } } [do -- continueWithCmd state { data { uploadedImagesIds = imageIds',  addedImages = images',  addImagesState { images = images', stateChanged = not (imageIds' == state.data.uploadedImagesIds), imageMediaIds = imageIds' } }, props  } [do
    void $ forWithIndex images' \i x -> do
      void $ runEffectFn4 renderBase64ImageFile x.image (getNewIDWithTag "add_image_component_image" <> (show i)) false "CENTER_CROP"
      pure NoAction
    pure $ Done
  ]

eval Done state = do
  continue state { data  { uploadedImagesIds = state.data.addImagesState.imageMediaIds, addedImages = state.data.addImagesState.images }
                 , props { showImageModel = false } }

eval _ state = update state
