{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.DocumentCaptureScreen.View where

import Animation as Anim
import Animation.Config
import PrestoDOM 
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeaders
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.String as DS
import Data.Maybe
import Data.Function.Uncurried (runFn1)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Prelude (Unit, const, ($), (<<<), (<>), bind, discard, unit, pure, map, (==), (/=), (>), not, show, (*), (+), (/), (-), (&&))
import PrestoDOM.Properties (cornerRadii)
import Engineering.Helpers.Commons as EHC
import Screens.DocumentCaptureScreen.Controller (Action(..), eval, ScreenOutput(..))
import Screens.DocumentCaptureScreen.ComponentConfig
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Font.Style as FontStyle
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import JBridge as JB
import PaymentPage (consumeBP)
import Effect.Uncurried (runEffectFn1)
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Components.ValidateDocumentModal as ValidateDocumentModal
import Components.PopUpModal as PopUpModal
import Screens.RegistrationScreen.ComponentConfig (logoutPopUp)
import Common.Types.App as CTA
import Resource.Constants as Constant
import Common.Animation.Config as AnimConfig
import Components.OptionsMenu as OptionsMenu
import Debug (spy)
import Screens.RegistrationScreen.ComponentConfig (changeVehicleConfig)
import Data.Array as DA
import Components.BottomDrawerList as BottomDrawerList
import Engineering.Helpers.Events as EHE
import Helpers.Utils as HU
import Mobility.Prelude
import Services.API as API
import Engineering.Helpers.Utils as EHU
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)


screen :: ST.DocumentCaptureScreenState -> Screen Action ST.DocumentCaptureScreenState ScreenOutput
screen initialState = 
  { initialState
  , view
  , name : "DocumentCaptureScreen"
  , globalEvents : [(\push -> do
    _ <- JB.storeCallBackImageUpload push CallBackImageUpload
    _ <- runEffectFn1 consumeBP unit
    let _ = EHE.addEvent (EHE.defaultEventObject $ HU.getRegisterationStepScreenLoadedEventName initialState.data.docType) { module = HU.getRegisterationStepModule initialState.data.docType, source = HU.getRegisterationStepScreenSource initialState.data.docType }
    -- _ <-
    --   launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
    --     $ do
    --         if isJust initialState.data.linkedRc && initialState.data.docType == ST.VEHICLE_PHOTOS then do
    --           (API.GetVehiclePhotosResp vehiclePhotosResp) <- Remote.getVehiclePhotosBase64BT (fromMaybe "" initialState.data.linkedRc) true true true true true true true
    --           lift $ lift $ doAff do liftEffect $ push $ UpdateVehiclePhotos (API.GetVehiclePhotosResp vehiclePhotosResp)
    --         else pure unit
    _ <-
      launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
        $ do
            if isJust initialState.data.linkedRc && initialState.data.docType == ST.VEHICLE_PHOTOS && initialState.props.uploadVehiclePhotos then do
              (API.GetVehiclePhotosResp vehiclePhotosResp) <- Remote.getVehiclePhotosBase64BT (fromMaybe "" initialState.data.linkedRc) true false false false false false false
              lift $ lift $ doAff do liftEffect $ push $ UpdateVehiclePhotosWithType (API.GetVehiclePhotosResp vehiclePhotosResp) API.VehicleFront
            else pure unit
    _ <-
      launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
        $ do
            if isJust initialState.data.linkedRc && initialState.data.docType == ST.VEHICLE_PHOTOS && initialState.props.uploadVehiclePhotos then do
              (API.GetVehiclePhotosResp vehiclePhotosResp) <- Remote.getVehiclePhotosBase64BT (fromMaybe "" initialState.data.linkedRc) false true false false false false false
              lift $ lift $ doAff do liftEffect $ push $ UpdateVehiclePhotosWithType (API.GetVehiclePhotosResp vehiclePhotosResp) API.VehicleBack
            else pure unit
    _ <-
      launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
        $ do
            if isJust initialState.data.linkedRc && initialState.data.docType == ST.VEHICLE_PHOTOS && initialState.props.uploadVehiclePhotos then do
              (API.GetVehiclePhotosResp vehiclePhotosResp) <- Remote.getVehiclePhotosBase64BT (fromMaybe "" initialState.data.linkedRc) false false true false false false false
              lift $ lift $ doAff do liftEffect $ push $ UpdateVehiclePhotosWithType (API.GetVehiclePhotosResp vehiclePhotosResp) API.VehicleRight
            else pure unit
    _ <-
      launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
        $ do
            if isJust initialState.data.linkedRc && initialState.data.docType == ST.VEHICLE_PHOTOS && initialState.props.uploadVehiclePhotos then do
              (API.GetVehiclePhotosResp vehiclePhotosResp) <- Remote.getVehiclePhotosBase64BT (fromMaybe "" initialState.data.linkedRc) false false false true false false false
              lift $ lift $ doAff do liftEffect $ push $ UpdateVehiclePhotosWithType (API.GetVehiclePhotosResp vehiclePhotosResp) API.VehicleLeft
            else pure unit
    _ <-
      launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
        $ do
            if isJust initialState.data.linkedRc && initialState.data.docType == ST.VEHICLE_PHOTOS && initialState.props.uploadVehiclePhotos then do
              (API.GetVehiclePhotosResp vehiclePhotosResp) <- Remote.getVehiclePhotosBase64BT (fromMaybe "" initialState.data.linkedRc) false false false false true false false
              lift $ lift $ doAff do liftEffect $ push $ UpdateVehiclePhotosWithType (API.GetVehiclePhotosResp vehiclePhotosResp) API.VehicleFrontInterior
            else pure unit
    _ <-
      launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
        $ do
            if isJust initialState.data.linkedRc && initialState.data.docType == ST.VEHICLE_PHOTOS && initialState.props.uploadVehiclePhotos then do
              (API.GetVehiclePhotosResp vehiclePhotosResp) <- Remote.getVehiclePhotosBase64BT (fromMaybe "" initialState.data.linkedRc) false false false false false true false
              lift $ lift $ doAff do liftEffect $ push $ UpdateVehiclePhotosWithType (API.GetVehiclePhotosResp vehiclePhotosResp) API.VehicleBackInterior
            else pure unit
    _ <-
      launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
        $ do
            if isJust initialState.data.linkedRc && initialState.data.docType == ST.VEHICLE_PHOTOS && initialState.props.uploadVehiclePhotos then do
              (API.GetVehiclePhotosResp vehiclePhotosResp) <- Remote.getVehiclePhotosBase64BT (fromMaybe "" initialState.data.linkedRc) false false false false false false true
              lift $ lift $ doAff do liftEffect $ push $ UpdateVehiclePhotosWithType (API.GetVehiclePhotosResp vehiclePhotosResp) API.Odometer_
            else pure unit
    pure $ pure unit
  )] 
  , eval :
      ( \state action -> do
          let _ = spy "DocumentCaptureScreen ----- state" state
          let _ = spy "DocumentCaptureScreen --------action" action
          eval state action
        ) 
  }

view :: forall w. (Action -> Effect Unit) -> ST.DocumentCaptureScreenState -> PrestoDOM (Effect Unit) w 
view push state = 
  Anim.screenAnimation $
  frameLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ] $ [ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , onBackPressed push $ const BackPressed 
      ][ AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)
        , linearLayout
          [ width MATCH_PARENT
          , weight 1.0
          , orientation VERTICAL
          ][  scrollView
              [ height MATCH_PARENT
              , width MATCH_PARENT
              ][ linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  , margin $ Margin 20 20 15 0
                  ] (if state.props.uploadVehiclePhotos then [uploadVehicleImageView push state] else [ howToUpload push state ])
                ]
          ]
        , if state.props.uploadVehiclePhotos then PrimaryButton.view (push <<< VehicleUploadPrimaryButtonAC) (vehicleUploadButtonConfig state) else PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
      ]
    , if state.props.contactSupportModal /= ST.HIDE then BottomDrawerList.view (push <<< BottomDrawerListAC) (bottomDrawerListConfig state) else linearLayout[][]
    ] <> if state.props.validateDocModal then [ValidateDocumentModal.view (push <<< ValidateDocumentModalAction) (validateDocModalState state)] else []
      <> if DA.any (_ == true) [state.props.logoutModalView, state.props.confirmChangeVehicle] then [ popupModal push state ] else []
      <> if state.props.menuOptions then [menuOptionModal push state] else []


menuOptionModal :: forall w. (Action -> Effect Unit) -> ST.DocumentCaptureScreenState -> PrestoDOM (Effect Unit) w
menuOptionModal push state = 
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingTop 55
    ][ OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) ]

popupModal :: forall w . (Action -> Effect Unit) -> ST.DocumentCaptureScreenState -> PrestoDOM (Effect Unit) w
popupModal push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    ][ PopUpModal.view (push <<< action) popupConfig ] 
    where 
      action = if state.props.logoutModalView then PopUpModalLogoutAction 
                else ChangeVehicleAC
      popupConfig = if state.props.logoutModalView then (logoutPopUp CTA.Language)
                    else changeVehicleConfig FunctionCall

uploadVehicleImageView :: forall w. (Action -> Effect Unit) -> ST.DocumentCaptureScreenState -> PrestoDOM (Effect Unit) w
uploadVehicleImageView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  ]
  [ linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] (DA.mapWithIndex (\index item -> vehicleImageRowView push state index item) vehicleImageRowItem)
  ]

vehicleImageRowView :: forall w. (Action -> Effect Unit) -> ST.DocumentCaptureScreenState -> Int -> {leftImage :: String, leftText :: Maybe API.VehicleImageType ,rightImage :: String, rightText :: Maybe API.VehicleImageType} -> PrestoDOM (Effect Unit) w
vehicleImageRowView push state index item = 
  let leftImageBounds = runFn1 JB.getLayoutBounds $ EHC.getNewIDWithTag $ "vehicle_image0" <> (show index)
      rightImageBounds = runFn1 JB.getLayoutBounds $ EHC.getNewIDWithTag $ "vehicle_image1" <> (show index)
      leftText = getText item.leftText
      rightText = getText item.rightText
      getLeftImage = (getImage item.leftText state.data.vehiclePhotos)
      getRightImage = getImage item.rightText state.data.vehiclePhotos
  in
  PrestoAnim.animationSet [Anim.translateInYAnim translateYAnimConfig { duration = 1 , fromY = 0, toY = 0} ]
  $ linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginVertical 4 4
  , onAnimationEnd push $ const AfterRender
  ]
  [ linearLayout [weight 1.0, gravity CENTER][
    linearLayout 
    [ width $ V ((EHC.screenWidth unit)/2 - 20)
    , height WRAP_CONTENT
    , visibility $ boolToVisibility $ isJust item.leftText
    , gravity CENTER
    ]
    [ relativeLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ]
      [ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ Margin 8 8 8 8
        , cornerRadius 8.0
        , background Color.blue500
        , orientation VERTICAL
        ]
        [ linearLayout
          [ height $ V 117
          , width $ V 156
          , id $ EHC.getNewIDWithTag $ "vehicle_image0" <> (show index)
          ][imageView 
          [ width $ V 156 
          , height $ V 117
          , imageWithFallback $ fetchImage FF_COMMON_ASSET item.leftImage
          , stroke $ "1," <> (EHU.getColorWithOpacity 70 Color.purple800)
          , dashWidth 4
          , gapWidth 4
          , cornerRadius 8.0
          , gravity CENTER
          , visibility $ boolToVisibility $ DS.null getLeftImage
          ]]
        , textView 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ leftText
            , color Color.black900
            , margin $ MarginVertical 8 6
            ]
        ]
      , linearLayout
        [ height $ V $ 117-- HU.getDefaultPixelSize leftImageBounds.height
        , width $ V $ 156 -- HU.getDefaultPixelSize leftImageBounds.width
        , margin $ Margin 8 8 8 8
        , gravity CENTER
        , orientation VERTICAL
        , visibility $ boolToVisibility $ DS.null getLeftImage
        , onClick push $ const $ UploadImageWihType item.leftText
        , clickable $ DS.null getLeftImage
        ]
        [ imageView 
          [ height $ V 40
          , width $ V 40
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_plus_purple_circle"
          ]
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width $ V $ 156-- HU.getDefaultPixelSize leftImageBounds.width
        , gravity RIGHT
        , margin $ Margin 0 16 12 0
        , visibility $ boolToVisibility $ not $ DS.null getLeftImage
        ]
        [ imageView
          [ height $ V 20
          , width $ V 20
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_pencil_circle"
          , gravity RIGHT
          , onClick push $ const $ UploadImageWihType item.leftText
          , clickable $ not $ DS.null getLeftImage
          ]
        ] 
      ]
    ]
    ]
  , linearLayout [weight 1.0, gravity CENTER][
    linearLayout 
    [ width $ V ((EHC.screenWidth unit)/2 - 20)
    , height WRAP_CONTENT
    , visibility $ boolToInvisibility $ isJust item.rightText
    ]
    [ relativeLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ]
      [ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , cornerRadius 8.0
        , gravity CENTER
        , margin $ Margin 8 8 0 8
        , background Color.blue500
        , orientation VERTICAL
        ]
        [ linearLayout
          [ height $ V 117
          , width $ V 156
          , id $ EHC.getNewIDWithTag $ "vehicle_image1" <> (show index)
          ][imageView 
          [ width $ V 156 
          , height $ V 117
          , imageWithFallback $ fetchImage FF_COMMON_ASSET item.rightImage
          , stroke $ "1," <> (EHU.getColorWithOpacity 70 Color.purple800)
          , dashWidth 4
          , gapWidth 4
          , cornerRadius 8.0
          , visibility $ boolToVisibility $ DS.null getRightImage
          ]]
        , textView 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ rightText
            , color Color.black900
            , margin $ MarginVertical 8 6
            ]
        ]
      , linearLayout
        [ height $ V $ 117 -- HU.getDefaultPixelSize rightImageBounds.height
        , width $ V $ 156-- HU.getDefaultPixelSize rightImageBounds.width
        , margin $ Margin 8 8 8 8
        , gravity CENTER
        , orientation VERTICAL
        , visibility $ boolToVisibility $ DS.null getRightImage
        , onClick push $ const $ UploadImageWihType item.rightText
        , clickable $ DS.null getRightImage
        ]
        [ imageView 
          [ height $ V 40
          , width $ V 40
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_plus_purple_circle"
          ]
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width $ V $ 156 -- HU.getDefaultPixelSize rightImageBounds.width
        , gravity RIGHT
        , margin $ Margin 0 16 12 0
        , visibility $ boolToVisibility $ not $ DS.null getRightImage
        ]
        [ imageView
          [ height $ V 20
          , width $ V 20
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_pencil_circle"
          , gravity RIGHT
          , onClick push $ const $ UploadImageWihType item.rightText
          , clickable $ not $ DS.null getRightImage 
          ]
        ]
      ]
    ]
    ]
  ]
  where getText text = 
          case text of
            Just API.VehicleFront -> getString FRONT_STR
            Just API.VehicleBack -> getString BACK
            Just API.VehicleLeft -> getString LEFT_STR
            Just API.VehicleRight -> getString RIGHT_STR
            Just API.VehicleFrontInterior -> getString FRONT_INTERIOR
            Just API.VehicleBackInterior -> getString REAR_INTERIOR
            Just API.Odometer_ -> getString ODOMETER_STR
            _ -> ""
        getImage text (API.GetVehiclePhotosResp photos) = 
          case text of
            Just API.VehicleFront -> if DA.length photos.front > 0 then (fromMaybe "" $ DA.head photos.front) else ""
            Just API.VehicleBack -> if DA.length photos.back > 0 then (fromMaybe "" $ DA.head photos.back) else ""
            Just API.VehicleLeft -> if DA.length photos.left > 0 then (fromMaybe "" $ DA.head photos.left) else ""
            Just API.VehicleRight -> if DA.length photos.right > 0 then (fromMaybe "" $ DA.head photos.right) else ""
            Just API.VehicleFrontInterior -> if DA.length photos.frontInterior > 0 then (fromMaybe "" $ DA.head photos.frontInterior) else ""
            Just API.VehicleBackInterior -> if DA.length photos.backInterior > 0 then (fromMaybe "" $ DA.head photos.backInterior) else ""
            Just API.Odometer_ -> if DA.length photos.odometer > 0 then (fromMaybe "" $ DA.head photos.odometer) else ""
            _ -> ""

howToUpload :: (Action -> Effect Unit) ->  ST.DocumentCaptureScreenState -> forall w . PrestoDOM (Effect Unit) w
howToUpload push state = 
  let text1 = case state.data.docType of 
                ST.VEHICLE_PHOTOS -> getString TAKE_A_CLEAR_PICTURE_OF_THE_VEHICLE_FROM_THE_SUGGESTED_ANGLES
                ST.PROFILE_PHOTO -> getString TAKE_A_CLEAR_PICTURE_OF_THE_VEHICLE_FROM_THE_SUGGESTED_ANGLES
                _ -> (getVarString CAPTURE_DOC_DESC_1 [Constant.transformDocText state.data.docType])
      text2 = case state.data.docType of 
                ST.VEHICLE_PHOTOS -> getString ENSURE_ONLY_YOUR_VEHICLE_IS_IN_THE_FRAME_WHEN_UPLOADING
                ST.PROFILE_PHOTO -> getString ONLY_YOUR_FACE_SHOULD_BE_IN_THE_FRAME_WHEN_UPLOADING_THE_IMAGE
                _ -> (getString ENSURE_ADEQUATE_LIGHT)
      text3 = case state.data.docType of 
                ST.VEHICLE_PHOTOS -> "" 
                ST.PROFILE_PHOTO -> ""
                _ -> getVarString CAPTURE_DOC_DESC_3 [Constant.transformDocText state.data.docType]
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ textView $ 
    [ text $ getString HOW_TO_UPLOAD
    , color Color.greyTextColor
    ] <> FontStyle.h3 TypoGraphy
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginVertical 0 10
    , padding $ PaddingVertical 16 16
    ][ 
      textView $ 
      [ text text1
      , color Color.black800
      , margin $ MarginBottom 18
      , visibility $ boolToVisibility $ not $ DS.null text1
      ] <> FontStyle.body3 TypoGraphy

      , textView $ 
      [ text text2
      , color Color.black800
      , margin $ MarginBottom 18
      , visibility $ boolToVisibility $ not $ DS.null text2
      ] <> FontStyle.body3 TypoGraphy

      , textView $ 
      [ text text3
      , margin $ MarginBottom 40
      , color Color.black800
      , visibility $ boolToVisibility $ not $ DS.null text3
      ] <> FontStyle.body3 TypoGraphy
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , cornerRadius 4.0
        , margin $ MarginTop 20
        , stroke $ "1," <> Color.borderGreyColor
        , padding $ Padding 16 16 16 0
        ][ rightWrongView true state
         , rightWrongView false state
         ]  
    ]
  ]

rightWrongView :: Boolean -> ST.DocumentCaptureScreenState -> forall w . PrestoDOM (Effect Unit) w
rightWrongView isRight state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , margin $ MarginBottom 16
  ][ imageView
    [ width $ V 120
    , height $ V 120
    , imageWithFallback $ fetchImage FF_ASSET $ sampleImage isRight state
    ]
  , case state.data.docType of
      ST.VEHICLE_PHOTOS -> rightWrongItemViewForVehicelePhotos state isRight
      ST.PROFILE_PHOTO -> rightWrongItemViewForProfilePhoto state isRight
      _ -> linearLayout [ 
              width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            , padding $ Padding 16 16 0 0
            , gravity CENTER
            ][ rightWrongItemView isRight $ if isRight then (getString CLEAR_IMAGE) else (getString BLURRY_IMAGE)
            , rightWrongItemView isRight $ if isRight then (getString CROPPED_CORRECTLY) else (getString WRONG_CROPPING)
            ]
  ]

rightWrongItemViewForVehicelePhotos :: ST.DocumentCaptureScreenState -> Boolean -> forall w . PrestoDOM (Effect Unit) w
rightWrongItemViewForVehicelePhotos state isRight =
  linearLayout [ 
      width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 16 16 0 0
    , gravity CENTER
    ][ rightWrongItemView isRight $ if isRight then (getString CLEAR_IMAGE) else (getString BLURRY_IMAGE)
     , rightWrongItemView isRight $ if isRight then (getString FULL_VIEW) else (getString AVOID_PEOPLE)
     , rightWrongItemView isRight $ if isRight then "" else getString CROPPED_AREA
    ]

rightWrongItemViewForProfilePhoto :: ST.DocumentCaptureScreenState -> Boolean -> forall w . PrestoDOM (Effect Unit) w
rightWrongItemViewForProfilePhoto state isRight =
  linearLayout [ 
      width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 16 16 0 0
    , gravity CENTER
    ][ rightWrongItemView isRight $ if isRight then (getString CLEAR_IMAGE) else (getString BLURRY_IMAGE)
     , rightWrongItemView isRight $ if isRight then (getString INSIDE_MARKED_AREA) else (getString NO_CLOSED_EYES)
     , rightWrongItemView isRight $ if isRight then "" else (getString MORE_THAN_ONE_FACE)
     , rightWrongItemView isRight $ if isRight then "" else (getString OUTSIDE_MARKED_AREA)
    ]

sampleImage :: Boolean -> ST.DocumentCaptureScreenState -> String
sampleImage isRight state = 
  case state.data.docType of
    ST.VEHICLE_PERMIT -> if isRight then "ny_ic_permit_clear" else "ny_ic_permit_blur"
    ST.FITNESS_CERTIFICATE -> if isRight then "ny_ic_fitness_clear" else "ny_ic_fitness_blur"
    ST.VEHICLE_INSURANCE -> if isRight then "ny_ic_insurance_clear" else "ny_ic_insurance_blur"
    ST.VEHICLE_PUC -> if isRight then "ny_ic_puc_clear" else "ny_ic_puc_blur"
    ST.VEHICLE_PHOTOS -> if isRight then "ic_vehicle_image_clear" else "ic_vehicle_image_blur"
    ST.PROFILE_PHOTO -> if isRight then "ny_ic_profile_photo_clear" else "ny_ic_profile_photo_blur"
    ST.AADHAAR_CARD -> if isRight then "ny_ic_adhar_clear" else "ny_ic_adhar_blur"
    ST.PAN_CARD -> if isRight then "ny_ic_pan_clear" else "ny_ic_pan_blur"
    _ -> if isRight then "ny_ic_upload_right" else "ny_ic_image_wrong"

rightWrongItemView :: Boolean -> String -> forall w . PrestoDOM (Effect Unit) w
rightWrongItemView isRight text' = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginBottom 5
  , gravity CENTER_VERTICAL
  , visibility $ boolToVisibility $ not $ DS.null text'
  ][ imageView
    [ width $ V 16
    , height $ V 16
    , imageWithFallback $ fetchImage FF_ASSET $ if isRight then "ny_ic_green_tick" else "ny_ic_payment_failed"
    ]
  , textView $
    [ text text'
    , color Color.black800
    , margin $ MarginLeft 8
    ] <> FontStyle.body1 TypoGraphy
  ]

vehicleImageRowItem :: Array {leftImage :: String, leftText :: Maybe API.VehicleImageType ,rightImage :: String, rightText :: Maybe API.VehicleImageType}
vehicleImageRowItem = [ {leftImage : "ic_vehicle_front_view", leftText : Just API.VehicleFront, rightImage : "ic_vehicle_back_view", rightText : Just API.VehicleBack},
                        {leftImage : "ic_vehicle_right_view", leftText : Just API.VehicleRight, rightImage : "ic_vehicle_left_view", rightText : Just API.VehicleLeft},
                        {leftImage : "ic_vehicle_front_interior_view", leftText : Just API.VehicleFrontInterior, rightImage : "ic_vehicle_rear_interior_view", rightText : Just API.VehicleBackInterior},
                        {leftImage : "ny_ic_odometer", leftText : Just API.Odometer_, rightImage : "", rightText : Nothing}]