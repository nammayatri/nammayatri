module Screens.DriverCompleteProfileScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Helpers.Utils (fetchImage, FetchImageFrom(..), getcurrentdate)
import Engineering.Helpers.Utils as EHU
import Language.Types (STR(..))
import Prelude (show, Unit, const, map, not, ($), (<<<), (<>), (==), (<>), (-), unit, (/=), (||), (+), bind, discard, void, pure, (/))
import Data.Array ( mapWithIndex, (..))
import PrestoDOM (onAnimationEnd, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), relativeLayout, afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine, scrollView, editText, hint, pattern, id, onChange, hintColor, multiLineEditText, nestedScrollView)
import Screens.DriverCompleteProfileScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.DriverCompleteProfileScreen.ComponentConfig
import Screens.Types as ST
import Styles.Colors as Color
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.Array as DA
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth)
import Components.PrimaryButton (view) as PrimaryButton
import Engineering.Helpers.Commons as EHC
import Font.Style (getFontStyle)
import Font.Style (Style(..))
import Data.String as DS
import Mobility.Prelude (boolToVisibility)
import JBridge as JB
import Effect.Uncurried (runEffectFn2)
import Components.AddImagesModel (view) as AddImagesModel
import Components.ViewImageModel.View (view) as ViewImageModel
import Effect.Uncurried(runEffectFn4, runEffectFn1)
import PrestoDOM.Animation as PrestoAnim
import Animation (scaleYAnimWithDelay, triggerOnAnimationEnd)
import Components.InputTextView.View (view) as InputTextView
import Services.Backend as Remote
import Effect.Aff (Milliseconds(..), launchAff)
import Services.API (DriverProfileDataRes(..))
import Types.App (GlobalState(..), defaultGlobalState, FlowBT)
import Common.Types.App (LazyCheck(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Data.Either (Either(..))
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM (FontWeight(..), fontStyle, lineHeight, textSize, fontWeight)
import Font.Style (bold, semiBold, medium)

screen :: ST.DriverCompleteProfileScreenState -> GlobalState -> Screen Action ST.DriverCompleteProfileScreenState ScreenOutput
screen initialState st =
  { initialState
  , view
  , name: "DriverProfileCompleteScreen"
  , globalEvents: [(\push ->  globalEvents' push)]
  , eval : ( \action state -> do
          let
            _ = spy "DriverProfileCompleteScreen action" action
          let
            _ = spy "DriverProfileCompleteScreen state" state
          eval action state
      )
  }
  where
  globalEvents' :: (Action -> Effect Unit) -> Effect (Effect Unit)
  globalEvents' push = do 
    void $ JB.storeCallBackImageUpload push ImageUploadCallback
    void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
    _ <- launchAff $ EHC.flowRunner st $ runExceptT $ runBackT $ getProfileData ProfileDataAPIResponseAction push initialState
    pure $ pure unit

getProfileData :: forall action. (DriverProfileDataRes -> action) -> (action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> FlowBT String Unit
getProfileData action push state = do
  void $ lift $ lift $ EHU.toggleLoader true
  driverProfileResp <- lift $ lift $ Remote.fetchDriverProfile $ Remote.makeDriverProfileDataReq
  case driverProfileResp of
      Right resp -> do
        liftFlowBT $ push $ action resp
        --pure $ resp
      Left _ -> void $ pure $ JB.toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
  void $ lift $ lift $ EHU.toggleLoader false
  pure unit

view :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout[
      height MATCH_PARENT
    , width MATCH_PARENT
  ]
  [
    scrollView[
      height MATCH_PARENT
    , width MATCH_PARENT
    , gravity $ CENTER_VERTICAL
    ][
        linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        ]
        [ header push state
        , seperator push state
        , uploadPhoto push state
        , uploadMorePhoto push state
        , pledge push state
        , vehicalOffer push state
        , homeTown push state
        , drivingSince push state
        , whyNy push state
        , PrimaryButton.view (push <<< OnClickDone) (donePrimaryButtonConfig state)
        ]
    ]
    , linearLayout[
        height MATCH_PARENT
      , width MATCH_PARENT
      , gravity CENTER
      , visibility $ boolToVisibility $ (state.props.showImageModel || state.props.showViewImageModel || state.props.showInputTextView)
      , background Color.blackLessTrans
        ](
          if state.props.showImageModel then [ addImageModel state push ] else []
        <> if state.props.showViewImageModel then [ viewImageModel state push ] else []
        <> if state.props.showInputTextView then [inputTextView state push ] else []
        )
  ] 


viewImageModel :: ST.DriverCompleteProfileScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
viewImageModel state push = ViewImageModel.view (push <<< ViewImageModelAction) (viewImageModelConfig state)

inputTextView :: ST.DriverCompleteProfileScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
inputTextView state push = InputTextView.view (push <<< InputTextAC) (inputTextConfig state)

header :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
header push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 20 20 0 16
    ][
        imageView
        [ width $ V 25
        , height $ V 25
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
        , margin $ MarginTop 3
        , onClick push $ const GoBack
        ]
    ,   textView
        $ [   text $ getString COMPLETE_YOUR_PROFILE
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ MarginLeft 20
            , color Color.black900
            ] <> FontStyle.h3 TypoGraphy
    ]

seperator :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
seperator push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   stroke $ "1," <> Color.grey900
    ][]

uploadPhoto :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
uploadPhoto push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 16 16 16
    ,   orientation VERTICAL
    ,   visibility $ boolToVisibility $ DA.length state.data.addedImages == 0
    ][
        textView
        $ [   text $ getString ADD_PHOTOS
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            , textSize FontSize.a_16
            , lineHeight "20.16"
            , fontWeight $ FontWeight 500
            , fontStyle $ medium LanguageStyle
            ]
    ,   linearLayout[
              width MATCH_PARENT
            , height WRAP_CONTENT
            , padding $ Padding 16 20 16 16
            , orientation VERTICAL
            , margin $ MarginTop 10
            , background Color.blue600
            , cornerRadius 8.0
        ][
            linearLayout[
                  width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity CENTER
            ][
                imageView
                [ width $ V 51
                , height $ V 51
                , gravity CENTER
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_upload_photo"
                , onClick push $ const OnClickUpload
                ]
            ]
        ,   textView $ 
            [     text $ getString ADD_UPTO_FOUR
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , color Color.black800
                , gravity CENTER
                , margin $ MarginTop 15
                ] <> FontStyle.body23 TypoGraphy

        ,   textView $ 
            [     text $ getString CARD_TEXT
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , color Color.black800
                , gravity CENTER
                , singleLine false
                , margin $ MarginTop 5
                ] <> FontStyle.body3 TypoGraphy
        ]
    ]

uploadMorePhoto :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
uploadMorePhoto push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 22 16 16
    ,   orientation VERTICAL
    ,   visibility $ boolToVisibility $ DA.length state.data.addedImages /= 0
    ][
        textView
        $ [   text $ getString ADD_PHOTOS <>" (" <> show ( DA.length state.data.addedImages ) <> "/4)"
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
    ,   textView
        $ [   text $ getString ADD_PHOTO_CAPTION
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black700
            , margin $ MarginTop 8
            ] <> FontStyle.body2 TypoGraphy
    ,   linearLayout[
              width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ MarginTop 25
        ] $ (mapWithIndex(\idx item -> addedImage push idx item.image) state.data.addedImages) <> 
            (mapWithIndex(\idx item -> leftImage push idx ) (1..(4 - (DA.length state.data.addedImages))))
    ]

addedImage :: forall w. (Action -> Effect Unit) -> Int -> String -> PrestoDOM (Effect Unit) w
addedImage push idx item = 
    relativeLayout[
        width WRAP_CONTENT
    ,   height WRAP_CONTENT
    ][
        linearLayout
        [ width $ V $ ((screenWidth unit) - 60) / (4)
        , height $ V 70
        , id (getNewIDWithTag ("driverImages" <> show idx))
        , margin $ MarginRight 5
        , cornerRadius 4.0
        , afterRender
            ( \action -> do 
                runEffectFn1 JB.displayBase64Image JB.displayBase64ImageConfig {source =  item, id = getNewIDWithTag ("driverImages" <> show idx), scaleType =  "FIT_XY", adjustViewBounds = false} 
            ) (const NoAction)
        ][]
    ,   imageView
        [ imageWithFallback $ fetchImage FF_ASSET $ "ny_ic_round_cross"
        , height $ V 19
        , width $ V 19
        , margin $ Margin 0 0 5 10
        , onClick push $ const $ OnClickDelete idx
        ]
    ]


leftImage :: forall w. (Action -> Effect Unit) -> Int -> PrestoDOM (Effect Unit) w
leftImage push _ = 
    linearLayout
    [ width $ V $ ((screenWidth unit) - 60) / (4)
    , height $ V 72
    , margin $ MarginRight 5
    , gravity CENTER
    , background Color.blue600
    , cornerRadius 4.0
    , stroke $ "1," <> Color.grey800
    ][
        imageView
        [ width $ V 33
        , height $ V 33
        , gravity CENTER
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_upload_photo"
        , onClick push $ const OnClickUpload
        ]
    ]

addImageModel :: ST.DriverCompleteProfileScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
addImageModel state push = AddImagesModel.view (push <<< AddImagesModelAction) (addImageModelConfig state)

pill :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> Action -> String -> Boolean -> PrestoDOM (Effect Unit) w
pill push state action title isSelected =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   cornerRadius 41.0
    ,   stroke $ "1," <> if isSelected == true then Color.blue900 else Color.grey900
    ,   padding $ Padding 0 8 0 8
    ,   margin $ Margin 0 0 16 13
    ,   onClick push $ const action 
    ,   background $ if isSelected == true then Color.blue600 else "#FFFFFF"
    ][
        textView $ 
        [     text $ title
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            , gravity CENTER
            ] <> FontStyle.body1 TypoGraphy
    ]

pillItems :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> Action -> String -> Boolean -> PrestoDOM (Effect Unit) w
pillItems push state action title isSelected =
    linearLayout[
        height WRAP_CONTENT
    ,   width $ V $ ((screenWidth unit) - 72) / 2
    ,   cornerRadius 41.0
    ,   stroke $ "1," <> if isSelected == true then Color.blue900 else Color.grey900
    ,   padding $ Padding 0 8 0 8
    ,   margin $ Margin 0 0 10 13
    ,   onClick push $ const $ action 
    ,   background $ if isSelected == true then Color.blue600 else "#FFFFFF"
    ][
        textView $ 
        [     text $ title
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            , gravity CENTER
            ] <> FontStyle.body1 TypoGraphy
    ]

pledge :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
pledge push state =
    let isSafeJourneySelected = checkPillSelected (getString SAFE_JOURNEY) state.data.pledge
        isCleanCarSelected = checkPillSelected (getString CLEAN_CAR) state.data.pledge
        isPickUpSelected = checkPillSelected (getString ON_TIME_PICK_UP) state.data.pledge
        isRegularMaintenanceSelected = checkPillSelected (getString MAINTENANCE) state.data.pledge
        isOtherSelected = checkPledgeOtherSelected state
    in 
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 12 16 16
    ,   orientation VERTICAL
    ][
        textView
        $ [   text $ getString PLEDGE
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
    ,   linearLayout[
              width MATCH_PARENT
            , height WRAP_CONTENT
            , padding $ Padding 16 13 0 0
            , orientation VERTICAL
            , margin $ MarginTop 14
            , cornerRadius 8.0
            , stroke $ "1," <> Color.grey900
        ][
            linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickPledge (getString SAFE_JOURNEY) isSafeJourneySelected ) (getString SAFE_JOURNEY) isSafeJourneySelected
            ,   pillItems push state (OnClickPledge (getString CLEAN_CAR) isCleanCarSelected) (getString CLEAN_CAR) isCleanCarSelected
            ]
        ,   pill push state (OnClickPledge (getString ON_TIME_PICK_UP) isPickUpSelected) (getString ON_TIME_PICK_UP) isPickUpSelected
        ,   pill push state (OnClickPledge (getString MAINTENANCE) isRegularMaintenanceSelected) (getString MAINTENANCE) isRegularMaintenanceSelected
        ,   pill push state (OnClickPledge (getString OTHER) isOtherSelected ) (getString OTHER) isOtherSelected
        ]
    ]

vehicalOffer :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
vehicalOffer push state =
    let isGasSelected = checkPillSelected (getString GAS) state.data.vehicalOffer
        isRadioSelected = checkPillSelected (getString RADIO) state.data.vehicalOffer
        isEcoFriendlySelected = checkPillSelected (getString ECO_FRIENDLY) state.data.vehicalOffer
        isDeviceChargingSelected = checkPillSelected (getString DEVICE_CHARGING) state.data.vehicalOffer
        isBootSpaceSelected = checkPillSelected (getString BOOT_SPACE) state.data.vehicalOffer
        isPetFriendly = checkPillSelected (getString PET_FRIENDLY) state.data.vehicalOffer
    in
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 12 16 16
    ,   orientation VERTICAL
    ][
        textView
        $ [   text $ (getString VEHICLE_OFFER)
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
    ,   linearLayout[
              width MATCH_PARENT
            , height WRAP_CONTENT
            , padding $ Padding 16 13 0 0
            , orientation VERTICAL
            , margin $ MarginTop 15
            , cornerRadius 8.0
            , stroke $ "1," <> Color.grey900
        ][
            linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickVehicalOffer (getString GAS) isGasSelected) (getString GAS) isGasSelected
            ,   pillItems push state (OnClickVehicalOffer (getString RADIO) isRadioSelected) (getString RADIO) isRadioSelected
            ]
        ,   linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickVehicalOffer (getString ECO_FRIENDLY) isEcoFriendlySelected) (getString ECO_FRIENDLY) isEcoFriendlySelected
            ,   pillItems push state (OnClickVehicalOffer (getString DEVICE_CHARGING) isDeviceChargingSelected) (getString DEVICE_CHARGING) isDeviceChargingSelected
            ]
        ,   linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickVehicalOffer (getString BOOT_SPACE) isBootSpaceSelected) (getString BOOT_SPACE) isBootSpaceSelected
            ,   pillItems push state (OnClickVehicalOffer (getString PET_FRIENDLY) isPetFriendly) (getString PET_FRIENDLY) isPetFriendly
            ]
        ]
    ]

homeTown :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
homeTown push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 12 16 16
    ,   orientation VERTICAL
    ][
        textView
        $ [   text $ getString FROM_WHERE
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , stroke ("1," <> Color.grey900)
          , cornerRadius 4.0
          , visibility VISIBLE
          , background Color.blue600
          , padding (Padding 16 2 16 2)
          , margin $ MarginTop 13
          ][
            ((if EHC.os == "ANDROID" then editText else multiLineEditText)
              $ [ width MATCH_PARENT
              , height ( V 40)
              , color Color.black800
              , hint ""
              , hintColor Color.black650
              , cornerRadius 4.0
              , text ""
              , singleLine false
              , onChange push (TextChanged ( EHC.getNewIDWithTag "") )
              , pattern "[A-Za-z0-9 ]*,100"
              ] <> (getFontStyle Body1 TypoGraphy))
            ]
    ]

drivingSince :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
drivingSince push state =
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 7 16 16
    ,   orientation VERTICAL
    ][
        textView
        $ [   text $ (getString DRIVING_SINCE)
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
    ,   linearLayout[
            height WRAP_CONTENT
        ,   width MATCH_PARENT
        ,   padding $ PaddingVertical 0 10
        ,   margin $ MarginTop 10
        ][
            scrollView
            [ height $ V 160
            , width MATCH_PARENT
            , id $ getNewIDWithTag state.data.datePickerState.id
            , stroke $ "1," <> Color.grey900
            , cornerRadius 8.0
            , nestedScrollView true
            , padding $ Padding 16 10 16 16
            ]
            [ linearLayout
                [ height $ V 137
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                ( mapWithIndex
                    ( \index item ->
                        linearLayout
                        [ height $ V 45
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        , id $ getNewIDWithTag (state.data.datePickerState.id <> show index)
                        , onClick push $ const $ OnDateSelect index item.year
                        ]
                        [ linearLayout[
                                height WRAP_CONTENT
                            ,   width MATCH_PARENT
                            ,   weight 1.0
                            ,   gravity CENTER
                            ][
                                textView
                                $ [   height WRAP_CONTENT
                                    , width WRAP_CONTENT
                                    , text $ show $ item.year
                                    , color if index == state.data.datePickerState.activeIndex then Color.black900 else "#A7A7A7"
                                    ]
                                <> FontStyle.h3 LanguageStyle
                            ]
                        ,   linearLayout[
                                height WRAP_CONTENT
                            ,   width MATCH_PARENT
                            ,   stroke $ "1," <> Color.grey900
                            ][]
                        ]
                    ) state.data.datePickerState.dates
                )
            ]
        ]
    ]

whyNy :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
whyNy push state =
    let isNewHomeSelected = checkPillSelected (getString NEW_HOME) state.data.whyNy
        isKidsEducationSelected = checkPillSelected (getString KID_EDUCATION) state.data.whyNy
        isNewVehicalSelected = checkPillSelected (getString NEW_VEHICLE) state.data.whyNy
        isOtherSelected = checkWhyNyOtherSelected state
    in
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 7 16 16
    ,   orientation VERTICAL
    ][
        textView
        $ [   text $ getString WHY_NY
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
    ,   linearLayout[
              width MATCH_PARENT
            , height WRAP_CONTENT
            , padding $ Padding 16 13 0 0
            , orientation VERTICAL
            , margin $ MarginTop 15
            , cornerRadius 8.0
            , stroke $ "1," <> Color.grey900
        ][
            linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickWhyNy (getString NEW_HOME) isNewHomeSelected) (getString NEW_HOME) isNewHomeSelected
            ,   pillItems push state (OnClickWhyNy (getString KID_EDUCATION) isKidsEducationSelected) (getString KID_EDUCATION) isKidsEducationSelected
            ]
        ,   linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickWhyNy (getString NEW_VEHICLE) isNewVehicalSelected) (getString NEW_VEHICLE) isNewVehicalSelected
            ,   pillItems push state (OnClickWhyNy (getString OTHER) isOtherSelected) (getString OTHER) isOtherSelected
            ]
        ]
    ]

checkPillSelected :: String -> Array String -> Boolean
checkPillSelected pill pillArray = DA.any (\item -> item == pill) pillArray

checkWhyNyOtherSelected :: ST.DriverCompleteProfileScreenState -> Boolean 
checkWhyNyOtherSelected state = if state.data.inputTextState.others.whyNy == "" then false else true

checkPledgeOtherSelected :: ST.DriverCompleteProfileScreenState -> Boolean 
checkPledgeOtherSelected state = if state.data.inputTextState.others.pledge == "" then false else true