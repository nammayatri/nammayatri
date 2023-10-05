{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.View where


import Screens.PaymentHistoryScreen.ComponentConfig

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Common.Types.App as Common
import Components.DueDetailsList (DueDetailsListState)
import Components.DueDetailsList as DueDetailsList
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except (lift, runExcept, runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String.CodeUnits (takeRight)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (convertUTCtoISC, flowRunner, screenWidth)
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style (Style(..), getFontStyle)
import Font.Style as FontStyle
import Foreign (Foreign, unsafeToForeign)
import Foreign.Generic (decode)
import Helpers.Utils (getAssetStoreLink, getFixedTwoDecimals)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, map, not, pure, show, unit, void, ($), (&&), (-), (<<<), (<>), (==), (>), (/), (/=), (||))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, VDom, Visibility(..), afterRender, alignParentBottom, background, backgroundColor, color, cornerRadius, fontStyle, gradient, gravity, height, imageView, imageWithFallback, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.PaymentHistoryScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.PaymentHistoryScreen.ScreenData (dummyPromoConfig)
import Screens.PaymentHistoryScreen.Transformer (getAutoPayPaymentStatus, getAutoPayStageData)
import Screens.SubscriptionScreen.Transformer (getPromoConfig)
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PromoConfig)
import Services.API (FeeType(..), GetPaymentHistoryResp(..), PaymentDetailsEntity(..), HistoryEntityV2Resp(..)) as SA
import Services.API (FeeType(..), OfferEntity(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: PaymentHistoryScreenState -> Screen Action PaymentHistoryScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "PaymentHistoryScreen"
  , globalEvents: [(\push -> do
      void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
        if initialState.props.subView == PaymentHistory then do
          lift $ lift $ EHU.loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
          lift $ lift $ EHU.toggleLoader true
          let tab = if initialState.props.autoPayHistory then "AUTOPAY_INVOICE" else "MANUAL_INVOICE"
          resp <- lift $ lift $ Remote.paymentHistoryListV2 "15" "0" tab
          case resp of
            Right (SA.HistoryEntityV2Resp resp) -> do
              lift $ lift $ doAff do liftEffect $ push $ UpdatePaymentHistory (SA.HistoryEntityV2Resp resp)
            Left err -> pure unit
          lift $ lift $ EHU.toggleLoader false
          pure unit
        else pure unit
      pure (pure unit)
    )]
  , eval:
      ( \state action -> do
          let _ = spy "PaymentHistoryScreen ----- state" state
          let _ = spy "PaymentHistoryScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimationFadeInOut $
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , onBackPressed push $ const BackPressed
  , afterRender push $ const AfterRender
  , background Color.white900
  ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey900
      , margin $ MarginBottom 16
      ][]
    , relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , weight 1.0
      , orientation VERTICAL
      ][  if (state.props.subView == PaymentHistory) then paymentHistoryView push state (state.props.subView == PaymentHistory) else emptyView
        , if (state.props.subView == TransactionDetails) then transactionDetails push state (state.props.subView == TransactionDetails) else emptyView
        , if (state.props.subView == RideDetails) then rideDetails push state (state.props.subView == RideDetails) else emptyView
      ]
  ]

emptyView :: forall w. PrestoDOM (Effect Unit) w
emptyView = linearLayout[][]

paymentHistoryView :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
paymentHistoryView push state visibility' = 
  PrestoAnim.animationSet [Anim.fadeIn visibility'] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16
  , visibility if visibility' then VISIBLE else GONE
  ][ tabView state push
   , paymentList push state
  ]

paymentList :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
paymentList push state = 
  let transactionItems = if state.props.autoPayHistory then state.data.autoPayList else state.data.manualPayList
  in
  PrestoAnim.animationSet [Anim.fadeIn true] $ 
  if DA.null transactionItems then noPaymentsView state push 
  else 
    scrollView
    [ width MATCH_PARENT
    , weight 1.0
    , height WRAP_CONTENT
    , scrollBarY false
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , visibility if DA.length transactionItems > 0 then VISIBLE else GONE
        ] (DA.mapWithIndex (\index item -> 
          let itemConfig = getStatusConfig item.paymentStatus
          in
          linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , stroke $ "1," <> Color.grey900
            , cornerRadius 8.0
            , margin $ MarginVertical 6 6
            , padding $ Padding 16 16 16 16
            , onClick push $ const $ ListItemClick item
            ][ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER_VERTICAL
                , orientation HORIZONTAL
                , margin $ MarginVertical 0 6
                ][ commonTV push (itemConfig.description <> "  ") Color.black700 (FontStyle.body3 TypoGraphy) 0 LEFT true
                , commonTV push (convertUTCtoISC item.transactionDate "Do MMM, YYYY") Color.black700 (FontStyle.body6 TypoGraphy) 0 LEFT true
                , linearLayout
                    [ height WRAP_CONTENT
                    , weight 1.0
                    ][]
                , commonTV push ("₹" <> getFixedTwoDecimals item.amount) itemConfig.color (FontStyle.h2 TypoGraphy) 0 RIGHT true
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , cornerRadius 24.0
                    , padding $ Padding 8 4 8 4
                    , background itemConfig.backgroundColor
                    , margin $ MarginLeft 4
                    ] [textView' push Nothing itemConfig.name itemConfig.color Body16 (Just $ PaddingTop 1) Nothing]
                ]
              , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                ][  commonTV push item.description Color.black700 (FontStyle.tags TypoGraphy) 0 CENTER true
                    , linearLayout
                        [ height WRAP_CONTENT
                        , weight 1.0
                        ][]
                    , linearLayout
                      [ height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , orientation HORIZONTAL
                      , gravity CENTER_VERTICAL
                      , cornerRadius 20.0
                      , background Color.white900
                      , padding $ Padding 8 5 8 5
                      ][ imageView
                          [ width $ V 12
                          , height $ V 12
                          , margin (MarginRight 4)
                          , imageWithFallback "ny_ic_upi_logo,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_upi_logo.png"
                          ]
                        , commonTV push (case item.feeType of 
                                          AUTOPAY_PAYMENT -> (getString UPI_AUTOPAY_S)
                                          AUTOPAY_REGISTRATION -> (getString UPI_AUTOPAY_SETUP)
                                          _ -> "UPI") Color.black700 (FontStyle.tags TypoGraphy) 0 CENTER true
                        ]
                ]
            ]
        ) transactionItems)
    ]

noPaymentsView :: forall w. PaymentHistoryScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
noPaymentsView state push =  
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  ][ imageView
    [ width $ V 200
    , height $ V 200
    , gravity CENTER
    , imageWithFallback case state.props.autoPayHistory, state.props.autoPaySetup of
                            true, true -> "ny_no_automatic_payments,"
                            true, false -> "ny_ic_enable_autopay," <> (getAssetStoreLink FunctionCall) <> "ny_ic_enable_autopay.png"
                            false, _ -> "ny_no_manual_payments,"
    ]
  , textView
    [ text $ getString $ case state.props.autoPayHistory, state.props.autoPaySetup of
                           true, true -> AUTOMATIC_PAYMENTS_WILL_APPEAR_HERE
                           true, false -> AUTOPAY_IS_NOT_ENABLED_YET
                           false, _ -> MANUAL_PAYMENTS_WILL_APPEAR_HERE
    , padding $ PaddingHorizontal 48 48
    , textSize FontSize.a_18
    , fontStyle $ FontStyle.bold LanguageStyle
    , color Color.black900
    , margin $ MarginBottom 10
    , gravity CENTER
    ]
  , textView
    [ text $ getString $ case state.props.autoPayHistory, state.props.autoPaySetup of
                           true, true -> NO_AUTOMATIC_PAYMENTS_DESC
                           true, false -> ENABLE_AUTOPAY_DESC
                           false, _ -> NO_MANUAL_PAYMENTS_DESC
    , padding $ PaddingHorizontal 48 48
    , textSize FontSize.a_14
    , fontStyle $ FontStyle.regular LanguageStyle
    , color Color.black700
    , gravity CENTER
    ]
  , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)
  ]

commonTV :: forall w. (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> Gravity -> Boolean -> PrestoDOM (Effect Unit) w
commonTV push text' color' fontStyle marginTop gravity' visibility'=
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , visibility if visibility' then VISIBLE else GONE
  ] <> fontStyle

textView' :: forall w. (Action -> Effect Unit) -> Maybe Action -> String -> String -> Style -> Maybe Padding -> Maybe Margin -> PrestoDOM (Effect Unit) w
textView' push action txt txtColor style padding' margin' =  
  textView $
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , color txtColor
  , text txt
  , gravity CENTER
  ] <> (getFontStyle style LanguageStyle)
    <> case padding' of  
         Just value -> [padding value]
         Nothing -> []
    <> case margin' of  
        Just value -> [margin value]
        Nothing -> []
    <> case action of  
        Just value -> [onClick push $ const $ value]
        Nothing -> []

tabView :: forall w. PaymentHistoryScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tabView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 24.0 
  , background Color.grey700
  , padding $ Padding 4 4 4 4
  , margin $ MarginBottom 28
  , gravity CENTER
  ][  textView
      [ height WRAP_CONTENT
      , weight 1.0 
      , background if state.props.autoPayHistory then Color.black900 else Color.grey700
      , text (getString AUTOPAY_PAYMENTS)
      , cornerRadius 24.0 
      , padding $ PaddingVertical 6 8
      , onClick push $ const $ ChangeTab true
      , fontStyle $ FontStyle.medium LanguageStyle
      , gravity CENTER
      , color if state.props.autoPayHistory then Color.white900 else Color.black700
      ]
    , textView
      [ height WRAP_CONTENT
      , weight 1.0 
      , gravity CENTER
      , cornerRadius 24.0  
      , onClick push $ const $ ChangeTab false
      , padding $ PaddingVertical 6 8
      , text (getString MANUAL_PAYMENTS)
      , fontStyle $ FontStyle.medium LanguageStyle
      , background if not state.props.autoPayHistory then Color.black900 else Color.grey700
      , color if not state.props.autoPayHistory then Color.white900 else Color.black700
      ]
  ]

transactionDetails :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
transactionDetails push state visibility' = 
  let config = getTransactionConfig state.data.transactionDetails
      autopayStageData = getAutoPayStageData state.data.transactionDetails.notificationStatus
      title = if state.data.transactionDetails.feeType /= AUTOPAY_PAYMENT then config.title else autopayStageData.stage
      statusTimeDesc = if state.data.transactionDetails.feeType /= AUTOPAY_PAYMENT then config.statusTimeDesc else autopayStageData.statusTimeDesc
  in
  PrestoAnim.animationSet [Anim.fadeIn visibility'] $
  scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , scrollBarY false
    , visibility if visibility' then VISIBLE else GONE
    , background Color.white900
    ][  linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            , margin $ MarginTop 20
            ][ imageView
                [ width $ V 114
                , height $ V 114
                , imageWithFallback config.image
                ]
              , commonTV push title Color.black900 (FontStyle.h2 TypoGraphy) 24 CENTER true
              , commonTV push ((statusTimeDesc) <> " : " <> state.data.transactionDetails.statusTime) Color.black700 (FontStyle.body3 TypoGraphy) 5 CENTER (state.data.transactionDetails.statusTime /= "")
            ]
          , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , margin $ Margin 16 20 16 0
              , padding $ Padding 16 8 16 8
              , cornerRadius  10.0
              , gravity CENTER_VERTICAL
              , background Color.blue600
              ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , padding $ PaddingVertical 8 8
                ](DA.mapWithIndex (\ index item ->
                  transactionHistoryRow push item.title index (DA.length state.data.transactionDetails.details) (item.val /= "") $ case item.key of
                    "OFFER" -> if item.val /= "" then promoCodeView push (fromMaybe dummyPromoConfig ((getPromoConfig [OfferEntity {title : Just item.val, description : Nothing, tnc : Nothing}]) DA.!! 0)) else rightItem push "N/A" false false
                    "TXN_ID" -> rightItem push item.val false true
                    "PAYMENT_MODE" -> rightItem push item.val true false
                    _ -> commonTV push item.val Color.black900 (FontStyle.body6 TypoGraphy) 0 RIGHT (item.val /= "")
                  ) state.data.transactionDetails.details)
            , manualPaymentRidesList push state -- if manualPayment
              ]
            , linearLayout [
                      width MATCH_PARENT
                    , height WRAP_CONTENT
                    , background Color.blue600
                    , margin $ Margin 16 8 16 24
                    , cornerRadius 4.0
                    , orientation VERTICAL
                 ][
                      textView $ [
                        text $ getString SWITCHED_TO_MANUAL
                        , width MATCH_PARENT
                        , margin $ MarginRight 8
                        , color Color.black600
                        , visibility if state.data.transactionDetails.isAutoPayFailed then VISIBLE else GONE
                        , padding $ Padding 8 8 8 8
                      ] <> FontStyle.tags TypoGraphy
                    , linearLayout
                      [
                        height $ V 1
                        , width MATCH_PARENT
                        , background Color.grey900
                        , margin $ MarginHorizontal 8 16
                        , visibility if state.data.transactionDetails.isAutoPayFailed && state.data.transactionDetails.isSplit then VISIBLE else GONE
                      ][]
                    , textView $ [
                        text $ getString SPLIT_PAYMENT
                        , width MATCH_PARENT
                        , margin $ MarginRight 8
                        , color Color.black600
                        , visibility if state.data.transactionDetails.isSplit then VISIBLE else GONE
                        , padding $ Padding 8 8 8 8
                      ] <> FontStyle.tags TypoGraphy
                 ]
        ]
    ]
  

transactionHistoryRow ∷ ∀ (a ∷ Type). (Action → Effect Unit) -> String -> Int -> Int -> Boolean -> VDom (Array (Prop (Effect Unit))) a -> VDom (Array (Prop (Effect Unit))) a 
transactionHistoryRow push item index length visibility' rightLayout  = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility if visibility' || item == getString OFFER then VISIBLE else GONE
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginTop 2
      , background Color.blue600
      , padding $ PaddingVertical 8 8
      , gravity CENTER_VERTICAL
      ][ commonTV push item Color.black700 (FontStyle.body3 TypoGraphy) 0 LEFT true
      , linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , gravity RIGHT
          ][ rightLayout ]
      ]
    , separatorView (index /= (length -1))
    ]

separatorView :: forall w. Boolean -> PrestoDOM (Effect Unit) w 
separatorView visible = 
  linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.white900
      , visibility if visible then VISIBLE else GONE
      ][]
promoCodeView :: forall w. (Action -> Effect Unit) -> PromoConfig -> PrestoDOM (Effect Unit) w 
promoCodeView push config =
  linearLayout
  ([ height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadius 100.0
  , padding $ Padding 10 4 10 4
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , margin $ MarginRight 4
  , gravity CENTER_VERTICAL
  ]<> if config.isGradient then [gradient (Linear 90.0 config.gradient)] else [])
   [ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if config.hasImage then VISIBLE else GONE
     , imageWithFallback config.imageURL
     ] 
   , textView $
     [ textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ] <> case config.title of
          Nothing -> [visibility GONE]
          Just txt -> [text txt]
  ]

rightItem :: forall w. (Action -> Effect Unit) -> String -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w 
rightItem push val prefixImage postfixImage =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , onClick push $ const if postfixImage then (Copy val) else NoAction
  ][ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if prefixImage then VISIBLE else GONE
     , imageWithFallback "ny_ic_upi_logo,"
     ] 
   , textView $
     [ text val
     , color Color.black900
     , padding $ PaddingBottom 3
     ] <> FontStyle.body6 TypoGraphy
  , imageView
     [ width $ V 16
     , height $ V 16
     , margin (MarginLeft 3)
     , visibility if postfixImage then VISIBLE else GONE
     , imageWithFallback $ "ny_ic_copy_blue," <> (getAssetStoreLink FunctionCall) <> "ny_ic_copy_blue.png"
     ] 
  ]

manualPaymentRidesList :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> PrestoDOM (Effect Unit) w
manualPaymentRidesList push state = 
  let screenwidth = (screenWidth unit) - 64
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , visibility if DA.null state.data.transactionDetails.manualSpecificDetails then GONE else VISIBLE
  , background Color.blue600
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginBottom 12
      , orientation HORIZONTAL
      ][ textView $
          [ text $ getString TRIP_DATE
          , width $ V (screenwidth/3)
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text $ getString PLAN
          , color Color.black700
          , width $ V (screenwidth/3)
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text $ "₹" <> getString AMOUNT
          , width $ V (screenwidth/3)
          , color Color.black700
          , gravity RIGHT
          ] <> FontStyle.body3 TypoGraphy
      ]
      , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginBottom 16
      ](map (\item -> 
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , margin $ MarginBottom 12
            ][ textView $
                [ text item.date
                , color Color.black800
                , width $ V (screenwidth/3)
                ] <> FontStyle.tags TypoGraphy
              , textView $
                [ text item.planType
                , width $ V (screenwidth/3)
                , color Color.black800
                ] <> FontStyle.tags TypoGraphy
              , textView $
                [ text ("₹" <> getFixedTwoDecimals item.dueAmount)
                , color Color.black800
                , width $ V (screenwidth/3)
                , gravity RIGHT
                ] <> FontStyle.tags TypoGraphy
            ]
            ) state.data.transactionDetails.manualSpecificDetails)
    , separatorView true
    , textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      , textFromHtml $ "<u>"<>(getString VIEW_RIDE_DETAILS)<>"</u>"
      , color Color.black650
      , onClick push $ const ViewRideDetails
      , padding $ PaddingVertical 16 16
      ] <> FontStyle.body1 TypoGraphy
    
  ]


rideDetails :: forall w. (Action -> Effect Unit) -> PaymentHistoryScreenState -> Boolean -> PrestoDOM (Effect Unit) w
rideDetails push state visibility' = 
  PrestoAnim.animationSet [Anim.fadeIn visibility'] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , visibility if visibility' then VISIBLE else GONE
  ][
    DueDetailsList.view (push <<< DueDetailsListAction) {dues : map (\item -> item{expanded = item.id == state.props.selectedDue}) state.data.transactionDetails.manualSpecificDetails}
  ]