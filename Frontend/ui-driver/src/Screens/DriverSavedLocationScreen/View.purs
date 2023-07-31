module Screens.DriverSavedLocationScreen.View where

import Data.Maybe
import Debug

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SavedLocationCard as SavedLocationCard
import Data.Array as DA
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Prelude (Unit, const, map, ($), (<<<), (<>), bind, pure, unit, (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, afterRender, alignParentBottom, alpha, background, color, cornerRadius, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, hintColor, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, weight, width)
import Screens.DriverSavedLocationScreen.ComponentConfig (primaryButtonConfig)
import Screens.DriverSavedLocationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (DriverSavedLocationScreenState, SavedLocationScreenType(..))
import Styles.Colors as Color

screen :: DriverSavedLocationScreenState ->  Screen Action DriverSavedLocationScreenState ScreenOutput
screen initialState = 
  { initialState
  , view
  , name : "DriverSavedLocationScreen"
  , globalEvents : [(\push -> do
    _ <- pure $ spy "globalEvents" ""
    pure $ pure unit
  )]
  , eval : 
        ( \state action -> do
          let _ = spy "DriverSavedLocationScreen ----- state" state
          let _ = spy "DriverSavedLocationScreen --------action" action
          eval state action
        ) 
  }


view :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ linearLayout 
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , background Color.white900
        ][ case state.props.viewType of 
            GO_TO_LIST -> gotoView state push 
            ADD_GO_TO_LOCATION -> addGoToLocation state push
            LOCATE_ON_MAP -> locateOnMap state push
            CONFIRM_LOCATION -> confirmLocation state push
            ENABLE_GO_TO -> enableGoTo state push
            _ -> addGoToLocation state push 
          ]

enableGoTo :: forall w.DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
enableGoTo state push = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , margin (Margin 0 15 0 0)
  ][ header'' state push 
   , linearLayout
     [ height $ V 1
     , width MATCH_PARENT
     , background Color.grey900
     ] [ ]
   , linearLayout
     [ height $ V 50  
     , width MATCH_PARENT
     , background Color.blue600
     , padding $ PaddingLeft 10
     , orientation VERTICAL
     , gravity CENTER
     ][ textView
        [ width MATCH_PARENT
        , text "Choose a Go-To location "
        ]
      ]  
   , savedLocationListView push state
   , linearLayout
      [ width MATCH_PARENT
      , orientation VERTICAL
      , gravity BOTTOM
      , weight 1.0
      ] [
        textView
          [ height $ V 45
          , width MATCH_PARENT
          , gravity CENTER
          , background Color.blue600
          , stroke $ "1,"<>Color.blue600
          , margin $ Margin 17 0 17 20 
          , text "You have only 2 left for today "
          , cornerRadius 6.0 
          , color Color.black900
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , margin $ MarginVertical 0 20
           ] [ textView
              [ height $ V 45
              , width $ V 160
              , background Color.white900
              , stroke $ "1,"<>Color.grey800
              , margin $ MarginHorizontal 17 17
              , text "Cancel" 
              , cornerRadius 6.0 
              , color Color.black900
              , gravity CENTER
              ]
            , textView
              [ height $ V 45
              , width $ V 160
              , background Color.black900
              , stroke $ "1,"<>Color.black800
              , margin $ MarginHorizontal 17 17
              , text "Yes Enable" 
              , color Color.yellow900
              , cornerRadius 6.0
              , gravity CENTER 
              ] 
            ]
          ]
       
   ]

gotoView :: forall w. DriverSavedLocationScreenState ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
gotoView state push =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , margin (Margin 0 15 0 0)
  ][ header state push 
   , savedLocationListView push state
   , linearLayout
      [ width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , gravity BOTTOM
      , weight 1.0
      ] [
        textView $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , text "Go-To locations left: "
          , color Color.black900
          ]
        , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)]
   ]


addGoToLocation :: forall w.DriverSavedLocationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
addGoToLocation state push = 
  frameLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , margin (Margin 0 EHC.safeMarginTop 0 0)
     , id (EHC.getNewIDWithTag "AddNewAddressHomeScreenMap")
      , afterRender
        (\action -> do
              _ <- (JB.showMap (EHC.getNewIDWithTag "AddNewAddressHomeScreenMap") true "satellite" (19.0) push MAPREADY)
              pure unit
              ) (const AfterRender)
     ][]
     , linearLayout
     [ height $ V 130
     , width MATCH_PARENT
     , background Color.white900
     ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginTop 10
        ] [ header state push  
          , textBox' push state
          ]
        , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , background Color.white900
          , cornerRadius 8.0
          , stroke "1,#E5E7EB"
          ] [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , cornerRadius 20.0
              , orientation VERTICAL
              , padding $ PaddingBottom 10
              ]( DA.mapWithIndex (\index item ->
                  linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ] [ linearLayout
                      [ height MATCH_PARENT
                      , width MATCH_PARENT
                      ] [ textView  
                          [ height MATCH_PARENT
                          , width MATCH_PARENT
                          , text item
                          ]
                        ]
                    , linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , background Color.lightGreyShade 
                      ][]   
                    ] 
                    ) ["Location 1", "Location 2" ,"Location 3"]
                )
            ]
      ]
   , linearLayout
     [ width MATCH_PARENT
     , height MATCH_PARENT
     , orientation HORIZONTAL
     , gravity BOTTOM
     ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL 
        , background Color.white900
        , padding $ Padding 20 20 20 20
        , gravity CENTER
        ][ imageView
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
            ][]
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
        ][]
      ]
  ]

locateOnMap :: forall w. DriverSavedLocationScreenState ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
locateOnMap state push = 
  frameLayout 
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , margin (Margin 0 EHC.safeMarginTop 0 0)
     , id (EHC.getNewIDWithTag "AddNewAddressHomeScreenMap")
      , afterRender
        (\action -> do
              _ <- (JB.showMap (EHC.getNewIDWithTag "AddNewAddressHomeScreenMap") true "satellite" (19.0) push MAPREADY)
              pure unit
              ) (const AfterRender)
     ][]
   , linearLayout
     [ height $ V 40
     , width $ V 40
     , background Color.white900
     , gravity CENTER
     , margin $ Margin 10 20 0 0 
     , cornerRadius 32.0  
     ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        ] [ imageView 
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
     ][ linearLayout
        [ width MATCH_PARENT
        , orientation VERTICAL
        , gravity BOTTOM
        , weight 1.0
        , background Color.white900
        , cornerRadius 12.0 
        ][ textView 
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
            , stroke $ "1,"<>Color.grey900
            , padding $ Padding 0 0 15 0
            , margin $ Margin 12 5 12 20
            , cornerRadius 6.0
            ] [ imageView
                [ height MATCH_PARENT
                , width WRAP_CONTENT
                , imageWithFallback "ny_ic_ellipse, https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
                , padding $ PaddingRight 10
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
                , afterRender (\action -> do
                    _ <- push action
                    _ <- pure $  EHC.getNewIDWithTag "SavedLocationEditText"
                    pure unit
                    ) (const NoAction)
                , ellipsize true
                , textSize FontSize.a_16
                , lineHeight "24"
                , onChange push OnTextChanged
                , hintColor "#A7A7A7" 
                ]
              ]
          , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
          ]

      ]
  ]



confirmLocation :: forall w. DriverSavedLocationScreenState ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
confirmLocation state push = 
  frameLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT 
  ][  linearLayout
      [ height $ V 150
      , width MATCH_PARENT
      ][ header' state push]
      , linearLayout
        [ orientation VERTICAL
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , margin (Margin 25 50 16 30)
        , padding (Padding 16 20 16 20)
        , cornerRadius 4.0
        , background Color.white900 
        ] [ textView
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
            , stroke $ "1,"<>Color.grey900
            , padding $ Padding 5 0 15 0
            , margin $ MarginVertical 5 20
            , cornerRadius 6.0
            ] [ editText
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
                , afterRender (\action -> do
                    _ <- push action
                    _ <- pure $  EHC.getNewIDWithTag "SavedLocationEditText"
                    pure unit
                    ) (const NoAction)
                , ellipsize true
                , textSize FontSize.a_16
                , lineHeight "24"
                , onChange push OnTextChanged
                , hintColor "#A7A7A7" 
                ]
              , textView
                [ text "Edit"
                , color Color.blue900
                , textSize FontSize.a_16
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
            , margin $ Margin 0 5 0 15
            , background Color.grey900
            , cornerRadius 4.0
            , gravity CENTER
            ] [ imageView
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
            , stroke $ "1,"<>Color.grey900
            , margin $ MarginTop 5
            , padding $ PaddingHorizontal 5 5 
            , cornerRadius 6.0
            ] [ editText
                [ height WRAP_CONTENT
                , color Color.black800
                , hint " "
                , fontStyle $ FontStyle.semiBold LanguageStyle
                , singleLine true
                , weight 1.0
                , cornerRadius 8.0
                , background Color.white900
                , text state.data.address
                , id $ EHC.getNewIDWithTag "SavedLocationEditText"
                , afterRender (\action -> do
                    _ <- push action
                    _ <- pure $  EHC.getNewIDWithTag "SavedLocationEditText"
                    pure unit
                    ) (const NoAction)
                , ellipsize true
                , textSize FontSize.a_16
                , lineHeight "24"
                , onChange push OnTextChanged
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
            ][ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state) ]
      ]   
    


    
header :: forall w. DriverSavedLocationScreenState ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding $ Padding 10 10 10 10
  , background Color.white900 
  ] [ imageView 
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
      ] [ textView $
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text "Add a Go-To Location"
          , gravity CENTER
          , color Color.black900
          ]
      ]
    ]

header'' :: forall w. DriverSavedLocationScreenState ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header'' state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding $ Padding 10 7 10 10
  , background Color.white900 
  ] [ imageView 
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
      ] [ textView $
          [ height WRAP_CONTENT
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
      ] [ textView
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text "Know More"
          , gravity RIGHT
          , color Color.blue900
          ]
        ]  
    ]


header' :: forall w. DriverSavedLocationScreenState ->  (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
header' state push = 
  linearLayout
  [ height $ V 150
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , padding $ Padding 10 20 10 20
  , background Color.black900
  ] [ imageView 
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
      ] [ textView $
          [ height WRAP_CONTENT
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
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (MarginTop 8)
          , orientation VERTICAL
          ](map (\item -> 
          SavedLocationCard.view (push <<< FavouriteLocationAC) (item) 
          ) (getFavourites))] 

textBox :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
textBox push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL 
  , background Color.white900
  , margin $ Margin 10 10 10 10
  , gravity CENTER
  ][  linearLayout
      [ orientation HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_HORIZONTAL
      , cornerRadius 8.0
      , stroke $ "1,"<>Color.grey900
      ][  imageView
          [ height $ V 16
          , width $ V 16
          , imageWithFallback "ic_nav_unfilled,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
          , onClick push $ const BackPressed
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
          , afterRender (\action -> do
              _ <- push action
              _ <- pure $  EHC.getNewIDWithTag "SavedLocationEditText"
              pure unit
              ) (const NoAction)
          , ellipsize true
          , padding $ Padding 21 27 16 27
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

textBox' :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
textBox' push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL 
  , background Color.white900
  , margin $ Margin 10 10 10 10
  , gravity CENTER
  ][  linearLayout
      [ orientation HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_HORIZONTAL
      , cornerRadius 8.0
      , stroke $ "1,"<>Color.grey900
      ][ linearLayout
        [width MATCH_PARENT
        , weight 1.0
        , gravity CENTER_VERTICAL
        , margin (MarginHorizontal 10 10)
        , padding (PaddingHorizontal 5 5) 
        ][ imageView
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
            , afterRender (\action -> do
                _ <- push action
                _ <- pure $  EHC.getNewIDWithTag "SavedLocationEditText"
                pure unit
                ) (const NoAction)
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
    -- , scrollView
    --   [ height MATCH_PARENT
    --   , width MATCH_PARENT 
    --   , cornerRadius 8.0
    --   , stroke "1,#E5E7EB"
    --   , background Color.white900
    --   , scrollBarY true
    --   ] [ linearLayout
    --       [ height MATCH_PARENT
    --       , width MATCH_PARENT
    --       , cornerRadius 20.0
    --       , orientation VERTICAL
    --       , padding $ PaddingBottom 10
    --       ]  (DA.mapWithIndex (\index item ->
    --             linearLayout
    --             [ width MATCH_PARENT
    --             , height WRAP_CONTENT
    --             , orientation VERTICAL
    --             ] [ linearLayout
    --                 [ width MATCH_PARENT
    --                 , height WRAP_CONTENT
    --                 ][ textView
    --                     [ width MATCH_PARENT
    --                     , height WRAP_CONTENT
    --                     , text item
    --                     ]
    --                 ]
    --               , linearLayout
    --                   [ height $ V 1
    --                   , width MATCH_PARENT
    --                   , background Color.lightGreyShade
    --                   ][]
    --               ]
    --               ) ["Lcation 1", "Location 2" ,"Location 3"] )
            
    --     ]
    ]

suggestionsView  :: forall w. (Action -> Effect Unit) -> DriverSavedLocationScreenState -> PrestoDOM (Effect Unit) w
suggestionsView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL 
  , background Color.white900
  , margin $ Margin 10 10 10 10
  , gravity CENTER
  ][  linearLayout
      [ orientation HORIZONTAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_HORIZONTAL
      , cornerRadius 8.0
      , stroke $ "1,"<>Color.grey900
      ][  imageView
          [ height $ V 16
          , width $ V 16
          , imageWithFallback "ny_ic_clear,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
          ]
        , editText
          [ height WRAP_CONTENT
          , color Color.black800
          , hint "Enter a location"
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , singleLine true
          , weight 1.0
          , cornerRadius 8.0
          , background Color.white900
          , text state.data.address
          , id $ EHC.getNewIDWithTag "SavedLocationEditText"
          , afterRender (\action -> do
              _ <- push action
              _ <- pure $ JB.requestKeyboardShow $ EHC.getNewIDWithTag "SavedLocationEditText"
              pure unit
              ) (const NoAction)
          , ellipsize true
          , padding $ Padding 21 27 16 27
          , textSize FontSize.a_16
          , lineHeight "24"
          , onChange push OnTextChanged
          , hintColor "#A7A7A7"
          ]
        , imageView
          [ height $ V 16
          , width $ V 16
          , imageWithFallback "ny_ic_clear,https://assets.juspay.in/nammayatri/images/user/ny_ic_clear.png"
          ]
       ]
  , scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , cornerRadius 8.0
    , stroke "1,#E5E7EB"
    , background Color.white900
    , scrollBarY false
    ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , cornerRadius 20.0
        , orientation VERTICAL
        , padding $ PaddingBottom 10
        ](DA.mapWithIndex (\index item ->
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][  linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  ][ textView
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , text item
                      ]
                  ]
                , linearLayout
                    [ height $ V 1
                    , width MATCH_PARENT
                    , background Color.lightGreyShade
                    ][]
              ]
              ) ["Lcation 1", "Location 2" ,"Location 3"] ) 
    ]
  ]

getFavourites :: Array Common.LocationListItemState
getFavourites = [
  {
   prefixImageUrl : ""
  , postfixImageUrl : ""
  , postfixImageVisibility : true
  , title : "HOME"
  , subTitle : "Stiring"
  , placeId : Nothing
  , lat : Nothing
  , lon : Nothing
  , description : "ADDRESS"
  , tag : "String"
  , tagType : Nothing
  , cardType : Nothing
  , address : "String"
  , tagName : "String"
  , isEditEnabled : true
  , savedLocation : "String"
  , placeName : "String"
  , isClickable : true
  , alpha : 1.0
  , fullAddress : address'
  , locationItemType : Nothing
  , distance : Nothing
  , showDistance : Nothing
  , editAcText : Just "Edit" 
  , removeAcText : Just "Remove"
  , radioButtonVisibility : false
  }
]

address' :: Common.Address 
address' = {
   area : Just "String"
  , state : Just "String"
  , country : Just "String"
  , building  :Just "String"
  , door : Just "String"
  , street : Just "String"
  , city : Just "String"
  , areaCode :Just "String"
  , ward : Just "String"
  , placeId : Just "String"
}



