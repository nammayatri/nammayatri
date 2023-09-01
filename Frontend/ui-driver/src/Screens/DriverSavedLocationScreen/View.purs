module Screens.DriverSavedLocationScreen.View where

import Data.Maybe
import Debug
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.GoToLocationModal as GoToLocationModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array (length)
import Data.Array as DA
import Data.Show
import Data.Either (Either(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, liftFlow)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils (loaderText, toggleLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, map, pure, unit, void, ($), (-), (<<<), (<>), (==), (>), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, alpha, background, color, cornerRadius, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.DriverSavedLocationScreen.ComponentConfig (confirmDeletePopupConfig, locationListItemConfig, primaryButtonConfig)
import Screens.DriverSavedLocationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (DriverSavedLocationScreenState, SavedLocationScreenType(..))
import Services.API (Prediction(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: DriverSavedLocationScreenState -> Screen Action DriverSavedLocationScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverSavedLocationScreen"
  , globalEvents:
      [ ( \push -> do
            if initialState.props.viewType == LOCATE_ON_MAP then
              JB.storeCallBackLocateOnMap push UpdateLocation
            else if initialState.props.viewType == GO_TO_LIST then do
              void $ launchAff $ flowRunner defaultGlobalState
                $ do
                    void $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                    toggleLoader true
                    resp <- Remote.getDriverHomeLocation ""
                    case resp of
                      Right response -> liftFlow $ push $ Respones response
                      Left error -> liftFlow $ push $ Error error
                    toggleLoader false
                    pure unit
            else
              pure unit
            pure $ pure unit
        )
      ]
  , eval:
      ( \state action -> do
          let
            _ = spy "DriverSavedLocationScreen ----- state" state
          let
            _ = spy "DriverSavedLocationScreen --------action" action
          eval state action
      )
  }

view :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , background Color.white900
        ]
        [ gotoLocationsList state push (state.props.viewType == GO_TO_LIST || state.props.viewType == NO_GO_TO_ADDED)
        , searchLocation state push (state.props.viewType == ADD_GO_TO_LOCATION)
        , locationAndMap state push (state.props.viewType == LOCATE_ON_MAP)
        , confirmLocation state push (state.props.viewType == CONFIRM_LOCATION)
        , confirmDeletePopup state push
        ]

confirmDeletePopup :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
confirmDeletePopup state push =
  PrestoAnim.animationSet [ Anim.fadeIn state.props.confirmDelete ]
    $ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.lightBlack900
        , visibility if state.props.confirmDelete then VISIBLE else GONE
        ]
        [ PopUpModal.view (push <<< PopUpModalAction) (confirmDeletePopupConfig state) ]

gotoLocationsList :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
gotoLocationsList state push visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ]
    $ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , visibility if visibility' then VISIBLE else GONE
        , margin (Margin 0 15 0 0)
        ]
        [ header state push
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 1
            , orientation VERTICAL
            , background Color.grey900
            , margin $ MarginTop 5
            ][]
        , savedLocationListView push state
        , savedLocationDefaultView push state
        , linearLayout
            [ width MATCH_PARENT
            , orientation VERTICAL
            , background Color.white900
            , gravity BOTTOM
            , weight 1.0
            ]
            [ textView
                $ [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , gravity CENTER
                  , text $ "Go-To locations left: " <> (show $ 5 - length state.data.savedLocationsArray)
                  , color Color.black900
                  ]
            , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
            ]
        ]

searchLocation :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
searchLocation state push visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ]
    $ frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , visibility if visibility' then VISIBLE else GONE
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.white900
            , orientation VERTICAL
            ]
            [ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , margin $ MarginTop 10
                ]
                [ header state push
                , textBox' push state
                , suggestionsListView state push
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation HORIZONTAL
            , gravity BOTTOM
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , background Color.white900
                , padding $ Padding 20 20 20 20
                , gravity CENTER
                ]
                [ imageView
                    [ height $ V 18
                    , width $ V 18
                    , imageWithFallback "ic_location_marker,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
                    ]
                , textView
                    [ height MATCH_PARENT
                    , color Color.black900
                    , text "Select on map"
                    , margin $ MarginHorizontal 13 13
                    , onClick push $ const LocateOnMap
                    ]
                , linearLayout
                    [ height MATCH_PARENT
                    , width $ V 2
                    , background Color.black800
                    , margin $ MarginHorizontal 10 10
                    ]
                    []
                , imageView
                    [ height $ V 18
                    , width $ V 18
                    , imageWithFallback "ny_ic_fill,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
                    ]
                , textView
                    [ height MATCH_PARENT
                    , color Color.black900
                    , text "Current Location "
                    , margin $ MarginHorizontal 13 13
                    ]
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                ]
                []
            ]
        ]

suggestionsListView :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
suggestionsListView state push =
  scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 8.0
    , stroke "1,#E5E7EB"
    , background Color.white900
    , scrollBarY false
    , margin $ Margin 10 10 10 10
    , visibility if length state.data.predictions > 0 then VISIBLE else GONE
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , cornerRadius 20.0
        , orientation VERTICAL
        , padding $ PaddingBottom 10
        ]
        ( DA.mapWithIndex
            ( \index item' ->
                let
                  (Prediction item) = item'
                in
                  linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    , onClick push $ const $ SuggestionClick item'
                    , margin $ MarginTop 10
                    ]
                    [ textView
                        [ width MATCH_PARENT
                        , height WRAP_CONTENT
                        , text item.description
                        , singleLine true
                        , padding $ Padding 10 10 10 10
                        ]
                    , linearLayout
                        [ height $ V 1
                        , width MATCH_PARENT
                        , margin $ MarginTop 10
                        , background Color.lightGreyShade
                        , visibility if index == (length state.data.predictions) - 1 then GONE else VISIBLE
                        ]
                        []
                    ]
            )
            state.data.predictions
        )
    ]

locationAndMap :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
locationAndMap state push visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ]
    $ frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , visibility if visibility' then VISIBLE else GONE
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , id (EHC.getNewIDWithTag "DriverSavedLoc")
            , afterRender
                ( \action -> do
                    _ <- (JB.showMap (EHC.getNewIDWithTag "DriverSavedLoc") true "satellite" (19.0) push MAPREADY)
                    pure unit
                )
                (const AfterRender)
            ]
            []
        , locateOnMap state push
        ]

locateOnMap :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
locateOnMap state push =
  frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        ]
        [ imageView
            [ width $ V 35
            , height $ V 35
            , imageWithFallback "ny_ic_src_marker,"
            ]
        ]
    , linearLayout
        [ height $ V 40
        , width $ V 40
        , background Color.white900
        , gravity CENTER
        , margin $ Margin 10 20 0 0
        , cornerRadius 32.0
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            ]
            [ imageView
                [ height $ V 40
                , width $ V 40
                , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
                , onClick push $ const BackPressed
                ]
            ]
        ]
    , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity BOTTOM
        , weight 1.0
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , orientation VERTICAL
            , gravity BOTTOM
            , weight 1.0
            , background Color.white900
            , cornerRadius 12.0
            ]
            [ textView
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , margin $ Margin 17 20 17 20
                , text "Confirm Go-To location"
                , textSize FontSize.a_20
                , color Color.black900
                , gravity CENTER
                ]
            , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , stroke $ "1," <> Color.grey900
                , padding $ Padding 0 10 15 10
                , margin $ Margin 16 5 16 20
                , cornerRadius 6.0
                ]
                [ imageView
                    [ height MATCH_PARENT
                    , width WRAP_CONTENT
                    , imageWithFallback "ny_ic_ellipse, https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
                    , padding $ PaddingRight 10
                    ]
                , textView
                    [ height WRAP_CONTENT
                    , color Color.black800
                    , fontStyle $ FontStyle.semiBold LanguageStyle
                    , singleLine true
                    , cornerRadius 8.0
                    , text state.data.saveLocationObject.address
                    ]
                ]
            , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
            ]
        ]
    ]

confirmLocation :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> Boolean -> PrestoDOM (Effect Unit) w
confirmLocation state push visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ]
    $ frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , visibility if visibility' then VISIBLE else GONE
        ]
        [ linearLayout
            [ height $ V 150
            , width MATCH_PARENT
            ]
            [ header' state push ]
        , linearLayout
            [ orientation VERTICAL
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , margin (Margin 25 50 16 30)
            , padding (Padding 16 20 16 20)
            , cornerRadius 4.0
            , background Color.white900
            ]
            [ textView
                [ text "Location"
                , color Color.black900
                , textSize FontSize.a_12
                , lineHeight "15"
                , fontStyle $ FontStyle.medium LanguageStyle
                , width MATCH_PARENT
                , gravity LEFT
                ]
            , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , stroke $ "1," <> Color.grey900
                , padding $ Padding 5 0 15 0
                , margin $ MarginVertical 5 20
                , cornerRadius 6.0
                ]
                [ textView
                    [ height WRAP_CONTENT
                    , color Color.black800
                    , text state.data.saveLocationObject.address
                    , fontStyle $ FontStyle.semiBold LanguageStyle
                    , singleLine true
                    , weight 1.0
                    , cornerRadius 8.0
                    , margin $ MarginLeft 5
                    , padding $ Padding 5 10 15 10
                    , ellipsize true
                    , textSize FontSize.a_16
                    ]
                , textView
                    [ text "Edit"
                    , color Color.blue900
                    , textSize FontSize.a_16
                    , onClick push $ const BackPressed
                    , visibility if state.props.fromEditButton then VISIBLE else GONE
                    ]
                ]
            , textView
                [ text "Add Tag (Only one location can have this tag)"
                , color Color.black900
                , textSize FontSize.a_12
                , lineHeight "15"
                , fontStyle $ FontStyle.medium LanguageStyle
                , width MATCH_PARENT
                , gravity LEFT
                ]
            , linearLayout
                [ height MATCH_PARENT
                , orientation HORIZONTAL
                , margin $ Margin 0 10 0 15
                , background Color.grey900
                , cornerRadius 4.0
                , gravity CENTER
                ]
                [ imageView
                    [ height $ V 12
                    , width $ V 12
                    , imageWithFallback "ny_ic_home,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
                    , margin $ MarginLeft 13
                    ]
                , textView
                    [ height WRAP_CONTENT
                    , width $ V 45
                    , text "Home"
                    , color Color.black800
                    , margin $ MarginLeft 10
                    , padding $ PaddingVertical 3 3
                    ]
                ]
            , textView
                [ text "Save As "
                , color Color.black900
                , textSize FontSize.a_12
                , lineHeight "15"
                , fontStyle $ FontStyle.medium LanguageStyle
                , width MATCH_PARENT
                , gravity LEFT
                ]
            , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation HORIZONTAL
                , stroke $ "1," <> Color.grey900
                , margin $ MarginTop 5
                , padding $ PaddingHorizontal 5 5
                , cornerRadius 6.0
                ]
                [ editText
                    [ height WRAP_CONTENT
                    , color Color.black800
                    , hint " "
                    , fontStyle $ FontStyle.semiBold LanguageStyle
                    , singleLine true
                    , weight 1.0
                    , cornerRadius 8.0
                    , background Color.white900
                    , text ""
                    , id $ EHC.getNewIDWithTag "ConfirmLocEDT"
                    , ellipsize true
                    , textSize FontSize.a_16
                    , lineHeight "24"
                    , onChange push ConfirmLocEDT
                    , hintColor "#A7A7A7"
                    ]
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            , gravity BOTTOM
            , weight 1.0
            ]
            [ PrimaryButton.view (push <<< ConfirmChangesAC) (primaryButtonConfig state) ]
        ]

header :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ Padding 10 10 10 10
    , background Color.white900
    ]
    [ imageView
        [ height MATCH_PARENT
        , width $ V 40
        , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
        , onClick push $ const BackPressed
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , padding $ PaddingLeft 15
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text "Add a Go-To Location"
              , gravity CENTER
              , color Color.black900
              ]
        ]
    ]

header'' :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header'' state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ Padding 10 7 10 10
    , background Color.white900
    ]
    [ imageView
        [ height MATCH_PARENT
        , width $ V 40
        , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
        , onClick push $ const BackPressed
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ PaddingLeft 10
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text "Enable Go-To"
              , gravity LEFT
              , color Color.black900
              ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingLeft 30
        ]
        [ textView
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text "Know More"
            , gravity RIGHT
            , color Color.blue900
            ]
        ]
    ]

header' :: forall w. DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header' state push =
  linearLayout
    [ height $ V 150
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ Padding 10 20 10 20
    , background Color.black900
    ]
    [ imageView
        [ height $ V 18
        , width $ V 18
        , imageWithFallback "ic_cancel_unfilled,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
        , onClick push $ const BackPressed
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , padding $ PaddingLeft 20
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width MATCH_PARENT
              , text "Go-To Locations"
              , gravity CENTER
              , color Color.white900
              ]
        ]
    ]

savedLocationListView :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
savedLocationListView push state =
  scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , scrollBarY true
    , visibility if state.data.savedLocationsArray == [] then GONE else VISIBLE
     ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin (MarginTop 8)
        , orientation VERTICAL
        ]
        (map (\item -> GoToLocationModal.view (push <<< GoToLocationModalAC) (locationListItemConfig item)) state.data.savedLocationsArray)
    ]

savedLocationDefaultView :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
savedLocationDefaultView push state =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , visibility if state.data.savedLocationsArray == [] then VISIBLE else GONE
    , gravity CENTER
    ][ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginTop 100
        ][ imageView
            [ height $ V 200
            , width MATCH_PARENT
            , imageWithFallback "ny_ic_banner_gender_feat,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
            , margin $ Margin 10 20 10 0
            ]
        , textView
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text "No Go-To locations added yet"
            , gravity CENTER
            , color Color.black900
            , margin $ Margin 10 20 10 0
            , fontStyle $ FontStyle.semiBold LanguageStyle
            ]
        , textView
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text "Go-To location helps you find rides in and around your preferred locations"
            , gravity CENTER
            , color Color.black800
            , margin $ Margin 10 20 10 0
            ]
        ]

        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , alignParentBottom "true,-1"
            , orientation VERTICAL
            ][ textView
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text "You have only 5 left for today."
                , gravity CENTER
                , color Color.black900
                ]
                , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
            ]
    ]


textBox' :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
textBox' push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , margin $ Margin 10 10 10 10
    , gravity CENTER
    ]
    [ linearLayout
        [ orientation HORIZONTAL
        , height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_HORIZONTAL
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , weight 1.0
            , gravity CENTER_VERTICAL
            , margin (MarginHorizontal 10 10)
            , padding (PaddingHorizontal 5 5)
            ]
            [ imageView
                [ height $ V 20
                , width $ V 20
                , imageWithFallback "ic_nav_unfilled,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
                ]
            , editText
                [ height WRAP_CONTENT
                , color Color.black800
                , hint "Location"
                , fontStyle $ FontStyle.semiBold LanguageStyle
                , singleLine true
                , weight 1.0
                , cornerRadius 8.0
                , background Color.white900
                , text state.data.address
                , id $ EHC.getNewIDWithTag "SavedLocationEditText"
                , afterRender
                    ( \action -> do
                        _ <- push action
                        _ <- pure $ EHC.getNewIDWithTag "SavedLocationEditText"
                        pure unit
                    )
                    (const NoAction)
                , ellipsize true
                , padding $ Padding 21 15 16 15
                , textSize FontSize.a_16
                , lineHeight "24"
                , onChange push OnTextChanged
                , hintColor "#A7A7A7"
                ]
            , imageView
                [ height $ V 16
                , width $ V 16
                , imageWithFallback "ic_cancel_unfilled,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
                ]
            ]
        ]
    ]
