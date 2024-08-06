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
import Prelude (show, Unit, const, map, not, ($), (<<<), (<>), (==), (<>), (-), unit, (/=), (||), (+), bind, discard, void, pure)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), relativeLayout, afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine, scrollView, editText, hint, pattern, id, onChange, hintColor, multiLineEditText)
import Screens.DriverCompleteProfileScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.DriverCompleteProfileScreen.ComponentConfig
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Data.Array as DA
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth)
import Components.PrimaryButton (view) as PrimaryButton
import Engineering.Helpers.Commons as EHC
import Font.Style (getFontStyle)
import Font.Style (Style(..))
import Data.String as DS
import Components.Calendar.View as Calendar
import Mobility.Prelude (boolToVisibility)
import JBridge as JB
import Effect.Uncurried (runEffectFn2)
import Components.AddImagesModel (view) as AddImagesModel
import Components.ViewImageModel.View (view) as ViewImageModel

screen :: ST.DriverCompleteProfileScreenState -> Screen Action ST.DriverCompleteProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverProfileCompleteScreen"
  , globalEvents: [(\push ->  globalEvents' push)]
  , eval
  }
  where
  globalEvents' :: (Action -> Effect Unit) -> Effect (Effect Unit)
  globalEvents' push = do 
    void $ JB.storeCallBackImageUpload push ImageUploadCallback
    void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
    pure $ pure unit

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
        ]
        [ header push state
        , seperator push state
        , uploadPhoto push state
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
        ](
          if state.props.showImageModel then [ addImageModel state push ] else []
        <> if state.props.showViewImageModel then [ viewImageModel state push ] else []
        )
  ] 


viewImageModel :: ST.DriverCompleteProfileScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
viewImageModel state push = ViewImageModel.view (push <<< ViewImageModelAction) (viewImageModelConfig state)

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
        $ [   text "Complete your profile"
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
    ][
        textView
        $ [   text "Add Your Photos"
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ] <> FontStyle.body33 TypoGraphy
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
            [     text "You can add upto 4 photos"
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , color Color.black800
                , gravity CENTER
                , margin $ MarginTop 15
                ] <> FontStyle.body23 TypoGraphy

        ,   textView $ 
            [     text "You can share photos of yourself, your family, and your vehicle to showcase to our customers!"
                , width MATCH_PARENT
                , height WRAP_CONTENT
                , color Color.black800
                , gravity CENTER
                , singleLine false
                , margin $ MarginTop 5
                ] <> FontStyle.body3 TypoGraphy
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
    ,   width $ V $ (screenWidth unit) - 245
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
    let isSafeJourneySelected = checkPillSelected "Safe Jouney" state.data.pledge
        isCleanCarSelected = checkPillSelected "Clean Car" state.data.pledge
        isPickUpSelected = checkPillSelected "On-time Pick up" state.data.pledge
        isRegularMaintenanceSelected = checkPillSelected "Regular Maintenance" state.data.pledge
        isOtherSelected = checkPillSelected "Other" state.data.pledge
    in 
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 12 16 16
    ,   orientation VERTICAL
    ][
        textView
        $ [   text "What I Pledge"
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
                pillItems push state (OnClickPledge "Safe Jouney" isSafeJourneySelected ) "Safe Jouney" isSafeJourneySelected
            ,   pillItems push state (OnClickPledge "Clean Car" isCleanCarSelected) "Clean Car" isCleanCarSelected
            ]
        ,   pill push state (OnClickPledge "On-time Pick up" isPickUpSelected) "On-time Pick up" isPickUpSelected
        ,   pill push state (OnClickPledge "Regular Maintenance" isRegularMaintenanceSelected) "Regular Maintenance" isRegularMaintenanceSelected
        ,   pill push state (OnClickPledge "Other" isOtherSelected ) "Other" isOtherSelected
        ]
    ]

vehicalOffer :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
vehicalOffer push state =
    let isGasSelected = checkPillSelected "Gas" state.data.vehicalOffer
        isRadioSelected = checkPillSelected "Radio" state.data.vehicalOffer
        isEcoFriendlySelected = checkPillSelected "Eco friendly" state.data.vehicalOffer
        isDeviceChargingSelected = checkPillSelected "Device Charging" state.data.vehicalOffer
        isBootSpaceSelected = checkPillSelected "Boot Space" state.data.vehicalOffer
        isPetFriendly = checkPillSelected "Pet Friendly" state.data.vehicalOffer
    in
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 12 16 16
    ,   orientation VERTICAL
    ][
        textView
        $ [   text "What my vehicle offers"
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
                pillItems push state (OnClickVehicalOffer "Gas" isGasSelected) "Gas" isGasSelected
            ,   pillItems push state (OnClickVehicalOffer "Radio" isRadioSelected) "Radio" isRadioSelected
            ]
        ,   linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickVehicalOffer "Eco friendly" isEcoFriendlySelected) "Eco friendly" isEcoFriendlySelected
            ,   pillItems push state (OnClickVehicalOffer "Device Charging" isDeviceChargingSelected) "Device Charging" isDeviceChargingSelected
            ]
        ,   linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickVehicalOffer "Boot Space" isBootSpaceSelected) "Boot Space" isBootSpaceSelected
            ,   pillItems push state (OnClickVehicalOffer "Pet Friendly" isPetFriendly) "Pet Friendly" isPetFriendly
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
        $ [   text "HomeTown"
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
        $ [   text "Driving Since"
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
    ,   linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , cornerRadius 8.0 
            , background Color.white900
            , stroke $ "1," <> Color.grey900
            , padding $ Padding 20 15 20 15
            , margin $ MarginTop 10
            , onClick (\action -> do
                      _ <- push action
                      JB.datePicker "" push $ DatePicker "DATE_OF_VISIT"
                ) (const NoAction)
            ][  imageView
                [ height $ V 22 
                , width $ V 22
                , margin $ MarginRight 8
                , layoutGravity "bottom"
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_calendar" 
                ]
              , textView $ 
                [ text $ if state.data.drivingSince == Nothing then "Select Date Of Visit" else (EHC.convertUTCtoISC (show $ fromMaybe 0 state.data.drivingSince) "dddFull, DD/MM/YY")
                , height WRAP_CONTENT
                , width WRAP_CONTENT
                , color Color.black800
                ] <> FontStyle.h3 TypoGraphy
            ]
    ]

whyNy :: forall w. (Action -> Effect Unit) -> ST.DriverCompleteProfileScreenState -> PrestoDOM (Effect Unit) w
whyNy push state =
    let isNewHomeSelected = checkPillSelected "Buy new home" state.data.whyNy
        isKidsEducationSelected = checkPillSelected "Kid’s Education" state.data.whyNy
        isNewVehicalSelected = checkPillSelected "Buy new vehicle" state.data.whyNy
        isOtherSelected = checkPillSelected "Other" state.data.whyNy
    in
    linearLayout[
        height WRAP_CONTENT
    ,   width MATCH_PARENT
    ,   margin $ Margin 16 7 16 16
    ,   orientation VERTICAL
    ][
        textView
        $ [   text "Why did i choose to drive auto?"
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
                pillItems push state (OnClickWhyNy "Buy new home" isNewHomeSelected) "Buy new home" isNewHomeSelected
            ,   pillItems push state (OnClickWhyNy "Kid’s Education" isKidsEducationSelected) "Kid’s Education" (checkPillSelected "Kid’s Education" state.data.whyNy)
            ]
        ,   linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                pillItems push state (OnClickWhyNy "Buy new vehicle" isNewVehicalSelected) "Buy new vehicle" isNewVehicalSelected
            ,   pillItems push state (OnClickWhyNy "Other" isOtherSelected) "Other" isOtherSelected
            ]
        ]
    ]

checkPillSelected :: String -> Array String -> Boolean
checkPillSelected pill pillArray = DA.any (\item -> item == pill) pillArray