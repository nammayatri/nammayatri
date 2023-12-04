{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.VehicleDetailsScreen.View where

import Prelude (Unit, const, map, not, ($), (<<<), (==), (<>), bind, pure, unit)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, layoutGravity, alpha, scrollView, cornerRadius, visibility, stroke, onBackPressed, afterRender, imageWithFallback)
import Effect (Effect)
import Screens.VehicleDetailsScreen.Controller (Action(..), ScreenOutput, eval, getTitle, getValue)
import Screens.Types as ST
import Screens.VehicleDetailsScreen.ScreenData (viewsItemList, ListOptions(..), Listtype)
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Data.Maybe(Maybe(..))
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.SelectVehicleTypeModal as SelectVehicleTypeModal
import JBridge as JB
import Common.Types.App
import Screens.VehicleDetailsScreen.ComponentConfig
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))

screen :: ST.VehicleDetailsScreenState -> ScopedScreen Action ST.VehicleDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "VehicleDetailsScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    pure $ pure unit)]
  , eval
  , parent: Nothing
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.VehicleDetailsScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , background Color.white900
    , afterRender push (const AfterRender)
    ][  linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ][ frameLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ][ PrestoAnim.animationSet 
                [ Anim.fadeOut state.props.isInEditVehicleDetailsView
                , Anim.fadeIn (not state.props.isInEditVehicleDetailsView)
                ] $ vehicleDetailsScreen state push
                , PrestoAnim.animationSet 
                [ Anim.fadeIn state.props.isInEditVehicleDetailsView
                , Anim.fadeOut (not state.props.isInEditVehicleDetailsView)
                ] $ editVehicleDetailsScreen state push 
                , if(state.props.isModalVisible) then (SelectVehicleTypeModal.view (push <<< SelectVehicleTypeModalAction) {title : (getString SELECT_VEHICLE_TYPE), listItems: state.data.vehicleTypes}) else dummyTextView
              ]
        ]
    ]



vehicleDetailsScreen :: ST.VehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
vehicleDetailsScreen state push = 
 linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginTop 10)
    , visibility  if state.props.isInEditVehicleDetailsView then GONE else VISIBLE 
    , alpha if state.props.isInEditVehicleDetailsView then 0.0 else 1.0
    ][ headerLayout state push (getString VEHICLE_DETAILS)
     , vehicleDetailsView state push
    ]

headerLayout :: ST.VehicleDetailsScreenState -> (Action -> Effect Unit) -> String -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push heading =
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 , margin (MarginTop 5)
 , layoutGravity "center_vertical"
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding (Padding 5 5 5 5)
    ][ imageView
        [ width $ V 25
        , height MATCH_PARENT
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
        , layoutGravity "center_vertical"
        , padding (Padding 2 2 2 2)
        , margin (MarginLeft 5)
        , onClick push (const BackPressed)
        ]
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text heading
        , margin (MarginLeft 20)
        , color Color.black
        , weight 1.0
        ] <> FontStyle.h3 LanguageStyle
      , textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString EDIT)
        , margin (MarginRight 10)
        , color Color.blueBtn
        , gravity RIGHT
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , visibility GONE -- if state.props.isInEditVehicleDetailsView then GONE else VISIBLE  TILL 15AUG
        , onClick push (const ToggleScreenMode)
        ]
    ]
  , linearLayout
    [ width MATCH_PARENT
    , height $ V 1 
    , background Color.black800
    , alpha 0.1
    , margin (MarginTop 10)
    ][]
 ]

vehicleDetailsView :: ST.VehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
vehicleDetailsView state push =
 scrollView
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (PaddingBottom 5)
    , margin (MarginTop 12)
    ] (map(\optionItem ->
            -- if(optionItem.title == VehicleRC) then (rcImageField optionItem push state) else HIDDEN TILL 15AUG
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , gravity CENTER_VERTICAL
              , margin (MarginTop 20)
              , visibility if(optionItem.title == VehicleRC) then GONE else VISIBLE
              ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , gravity CENTER_VERTICAL
                , padding (Padding 15 0 15 0)
                ][  textView
                    [ height WRAP_CONTENT
                    , text (getTitle optionItem.title)
                    , color Color.black800
                    , alpha 0.8
                    ]
                    , textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , margin (MarginTop 10)
                    , text (getValue optionItem.title state)
                    , color Color.black800
                    ] <> FontStyle.subHeading1 TypoGraphy
                ]
              , if(optionItem.title == VehicleColor ) then dummyTextView else horizontalLineView
            ]
          ) viewsItemList
    )
 ]

rcImageField :: Listtype -> (Action -> Effect Unit) -> ST.VehicleDetailsScreenState -> forall w . PrestoDOM (Effect Unit) w
rcImageField optionItem push state = 
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER_VERTICAL
  , padding (Padding 15 0 15 0)
  , gravity CENTER_VERTICAL
  , margin (MarginTop 20)
  ][ textView
      [ height WRAP_CONTENT
      , text (getTitle optionItem.title)
      , color Color.black800
      , alpha 0.8
      ]
      , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin (MarginTop 10)
      , orientation HORIZONTAL
      ][ textView $
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , text state.data.imageName
         , color Color.black800
         ] <> FontStyle.subHeading1 TypoGraphy
       , textView $                   
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , text (getString PREVIEW)
         , color Color.blueTextColor
         , margin (MarginLeft 10)
         , onClick push (const PreviewImage)
         ] <> FontStyle.subHeading1 TypoGraphy
      ]
  ]

horizontalLineView :: forall w . PrestoDOM (Effect Unit) w
horizontalLineView = 
 linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , background Color.black500
  , margin (Margin 15 20 15 0)
  , alpha 0.5
  ][]


dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView = 
 textView
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 ]


-------------------------------------------- editVehicleDetailsScreen ---------------------------------------------
editVehicleDetailsScreen :: ST.VehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
editVehicleDetailsScreen state push =
 linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin (MarginTop 10)
    , visibility  if state.props.isInEditVehicleDetailsView then VISIBLE else GONE 
    , alpha if state.props.isInEditVehicleDetailsView then 1.0 else 0.0
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , weight 1.0
        ][ headerLayout state push (getString UPDATE_VEHICLE_DETAILS)
        ,  updateVehicleDetailsFields state push]
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
    ]

updateVehicleDetailsFields :: ST.VehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
updateVehicleDetailsFields state push =
 scrollView
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , margin (MarginTop 25)
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (Padding 5 0 5 5)
    ] (map(\optionItem ->
            if(optionItem.title == VehicleType) then vehicleTypeView push state
                else if(optionItem.title == VehicleRC) then uploadRCView state push
                else PrimaryEditText.view (push <<< PrimaryEditTextActionController) (primaryEditTextConfig (getTitle optionItem.title) (getValue optionItem.title state))
          ) viewsItemList
    )
 ]


vehicleTypeView :: forall w. (Action -> Effect Unit) -> ST.VehicleDetailsScreenState -> PrestoDOM (Effect Unit) w
vehicleTypeView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (Padding 10 0 10 0)
    ][ textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , text (getString VEHICLE_TYPE)
        , color Color.greyTextColor
        ] <> FontStyle.paragraphText TypoGraphy
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , padding (Padding 20 16 20 16)
        , margin (Margin 0 10 0 10)
        , cornerRadius 6.0  
        , stroke ("1," <> Color.borderColorLight)
        , background Color.white900
        , onClick push (const SelectVehicleType)
        ][ textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , weight 1.0
            , text state.data.vehicleType
            , color Color.greyTextColor
            , alpha 0.8
            ] <> FontStyle.body7 TypoGraphy
          , imageView
            [ width ( V 15 )
            , height ( V 15 )
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_drop_down"
            ]
        ]
    ]

uploadRCView :: ST.VehicleDetailsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
uploadRCView state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding (Padding 10 0 10 0)
    , orientation VERTICAL
    ][ textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text (getString UPLOAD_REGISTRATION_CERTIFICATE)
        , color Color.greyTextColor
        ] <> FontStyle.paragraphText TypoGraphy
        , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , stroke ("1," <> Color.borderColorLight)
        , cornerRadius 6.0
        , gravity CENTER_VERTICAL
        , padding (Padding 20 16 20 16)
        , margin (Margin 0 10 0 10)
        , background Color.white900
        ][ textView $
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , weight 1.0
            , text if (state.props.deleteButtonVisibility) then state.data.imageName else (getString UPLOAD_RC)
            , alpha if (state.props.deleteButtonVisibility) then 0.8 else 0.5
            , color Color.greyTextColor
            ] <> FontStyle.body7 TypoGraphy
          , linearLayout
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            ][ textView $
               [ width WRAP_CONTENT
               , height MATCH_PARENT
               , text (getString PREVIEW)
               , color Color.blueBtn
               , onClick push (const PreviewImage)
               , visibility if state.props.deleteButtonVisibility then VISIBLE else GONE
               ] <> FontStyle.body5 TypoGraphy
             , imageView
               [ width ( V 20 )
               , height ( V 20 )
               , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if (state.props.deleteButtonVisibility) then "ny_ic_cancel" else "ny_ic_upload"
               , padding (Padding 2 2 2 0)
               , margin (MarginLeft 5)
               , onClick push $ if (state.props.deleteButtonVisibility) then ((const RemoveImageClick)) else ((const UploadImage))
               ]

            ]
        ]
    ]

