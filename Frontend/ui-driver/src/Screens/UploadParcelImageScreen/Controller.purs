module Screens.UploadParcelImageScreen.Controller where

import Prelude
import Data.Maybe
import Data.String
import JBridge as JB
import Debug (spy)
import Components.PrimaryButton.Controller as PrimaryButtonController
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, toast, updateAndExit)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Storage (KeyStore(..), getValueToLocalStore)
import Screens.Types as ST
import Engineering.Helpers.Commons as EHC
import Services.EndPoints as EP
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn3)

instance showAction :: Show Action where
    show _ =  ""
    
instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        NoAction -> pure unit
        AfterRender -> pure unit
        BackPressed -> pure unit
        CallBackImageUpload image imageName imagePath -> pure unit
        UploadMultiPartDataCallback fileType fileId -> pure unit
        ConfirmAndUpload action -> pure unit

data ScreenOutput = GoBack ST.UploadParcelImageScreenState | UploadImage ST.UploadParcelImageScreenState

data Action = AfterRender | BackPressed | NoAction | CallBackImageUpload String String String | ConfirmAndUpload PrimaryButtonController.Action | UploadMultiPartDataCallback String String

eval :: Action -> ST.UploadParcelImageScreenState -> Eval Action ScreenOutput ST.UploadParcelImageScreenState
eval NoAction state = continue state
eval AfterRender state = do
    continueWithCmd state { props {showConfirmAndUploadButton = false} } [do
            void $ liftEffect $ JB.renderCameraPicture (EHC.getNewIDWithTag "captureParcelImageView") "PARCEL_PICTURE"
            pure NoAction]
eval (BackPressed) state = do
    if state.props.showConfirmAndUploadButton then do
        continueWithCmd state { props {showConfirmAndUploadButton = false} } [do
            void $ liftEffect $ JB.renderCameraPicture (EHC.getNewIDWithTag "captureParcelImageView") "PARCEL_PICTURE"
            pure NoAction]
    else exit $ GoBack state
eval (CallBackImageUpload image imageName imagePath) state = do
    continueWithCmd state { data {imagePath = imagePath}, props { showConfirmAndUploadButton = true }} [do
        void $ pure $ JB.renderBase64Image image (EHC.getNewIDWithTag "confirmImageView") true "CENTER_CROP"
        pure NoAction
    ]
eval (UploadMultiPartDataCallback fileType fileId) state = do
    exit $ UploadImage state

eval (ConfirmAndUpload (PrimaryButtonController.OnClick)) state = do
    continueWithCmd state { props { uploading = true}} [do
        void $  runEffectFn3 JB.uploadMultiPartData state.data.imagePath (EP.uploadParcelImage state.data.rideId) "Image"
        pure NoAction
      ]

eval (ConfirmAndUpload _) state = continue state