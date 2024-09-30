module Screens.UploadParcelImageScreen.Controller where

import Prelude
import Data.Maybe
import Data.String
import JBridge as JB
import Debug (spy)
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.GenericHeader as GenericHeaderController
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, toast, updateAndExit)
import Screens (ScreenName(..), getScreen)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import PrestoDOM.Types.Core (class Loggable)
import Storage (KeyStore(..), getValueToLocalStore)
import Screens.Types as ST
import Engineering.Helpers.Commons as EHC
import Services.EndPoints as EP
import Effect.Class (liftEffect)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Effect.Uncurried (runEffectFn3, runEffectFn5)

instance showAction :: Show Action where
    show _ =  ""
    
instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        NoAction -> pure unit
        AfterRender -> trackAppScreenRender appId "screen" (getScreen UPLOAD_PARCEL_IMAGE_SCREEN)
        BackPressed -> pure unit
        CallBackImageUpload image imageName imagePath -> pure unit
        UploadMultiPartDataCallback fileType fileId -> pure unit
        _ -> pure unit

data ScreenOutput = GoBack ST.UploadParcelImageScreenState | UploadImage ST.UploadParcelImageScreenState

data Action = AfterRender | BackPressed | NoAction | CallBackImageUpload String String String | PrimaryButtonAC PrimaryButtonController.Action | UploadMultiPartDataCallback String String | GenericHeaderAC GenericHeaderController.Action

eval :: Action -> ST.UploadParcelImageScreenState -> Eval Action ScreenOutput ST.UploadParcelImageScreenState

eval AfterRender state = continue state 

eval BackPressed state = do
    if state.props.showConfirmAndUploadButton then do
        continue state { props {showConfirmAndUploadButton = false} }
    else exit $ GoBack state

eval (CallBackImageUpload image imageName imagePath) state = do
    continueWithCmd state { data {imagePath = (imagePath)}, props { showConfirmAndUploadButton = true }} [do
        void $ pure $ JB.renderBase64Image image (EHC.getNewIDWithTag "confirmImageView") true "CENTER_CROP"
        pure NoAction
    ]

eval (UploadMultiPartDataCallback _ _ ) state = do
    exit $ UploadImage state

eval (PrimaryButtonAC (PrimaryButtonController.OnClick)) state = do
    if state.props.showConfirmAndUploadButton then do
        continueWithCmd state { props { uploading = true}} [do
            void $  runEffectFn5 JB.uploadMultiPartData state.data.imagePath (EP.uploadParcelImage state.data.rideId) "Image" "result" "file"
            pure NoAction
        ]
    else continueWithCmd state { props {showConfirmAndUploadButton = false} } [do
            void $ liftEffect $ JB.takePhoto
            pure NoAction]

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = 
    if state.props.showConfirmAndUploadButton then do
        continue state { props {showConfirmAndUploadButton = false} }
    else exit $ GoBack state

eval _ state = update state