module Screens.TicketInfoScreen.View where

import Animation as Anim 
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import JBridge as JB 
import Prelude (Unit, bind, const, pure, unit, ($), (&&), (/=), (<<<),(<>), (==), map, show, (||), show, (-))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, afterRender, background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, orientation, padding, scrollView, text, textSize, textView, weight, width, imageWithFallback, cornerRadius, relativeLayout, alignParentBottom, layoutGravity, stroke, visibility, textFromHtml, onClick, clickable, id, horizontalScrollView)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketInfoScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
-- import Screens.TicketBookingScreen.ComponentConfig 
import Effect (Effect)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Font.Style as FontStyle
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag)
import Helpers.Utils (convertUTCToISTAnd12HourFormat, fetchImage, FetchImageFrom(..), decodeError, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getSearchType, parseFloat, storeCallBackCustomer)
import Services.API (BookingStatus(..), TicketPlaceResp(..), PlaceType(..))
import Animation (fadeInWithDelay, translateInXBackwardAnim, translateInXBackwardFadeAnimWithDelay, translateInXForwardAnim, translateInXForwardFadeAnimWithDelay)
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array as DA
import Data.Maybe (fromMaybe, Maybe(..), maybe)
import Debug
import Data.List ((:))
import Effect.Uncurried  (runEffectFn1)
import PaymentPage (consumeBP)

screen :: ST.TicketInfoScreenState -> Screen Action ST.TicketInfoScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "TicketInfoScreen"
  , globalEvents : [(\_ -> pure $ runEffectFn1 consumeBP unit)]
  , eval :
    \action state -> do
        let _ = spy "TicketInfoScreen action " action
        let _ = spy "TicketInfoScreen state " state
        eval action state
  }

view :: forall w . (Action -> Effect Unit) -> ST.TicketInfoScreenState -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet [Anim.fadeIn true] $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , onBackPressed push $ const BackPressed
   ][
    linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , margin $ MarginBottom 84
    ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , linearLayout
      [ height $ V 1 
      , width MATCH_PARENT
      , background Color.grey900][]
    , separatorView Color.greySmoke
    , scrollView[
        height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      ][ individualBookingInfoView state push]
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    , background Color.white900
    , orientation VERTICAL
    ][  linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900][]
      , textView $
        [ text "Go Home"
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , onClick push $ const GoHome
        , gravity CENTER
        , padding $ PaddingVertical 16 16
        , color Color.black900
        ] <> (FontStyle.subHeading1 TypoGraphy)
    ]
  ]

genericHeaderConfig :: ST.TicketInfoScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
      , height = V 25
      , width = V 25
      , margin = Margin 16 16 16 16
      } 
    , padding = PaddingVertical 5 5
    , textConfig {
        text = state.data.selectedBookingInfo.ticketPlaceName
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

separatorView :: forall w. String -> PrestoDOM (Effect Unit) w
separatorView color =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background color
  ][]

individualBookingInfoView :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
individualBookingInfoView state push =
 linearLayout
  [ height WRAP_CONTENT
  , width $ MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 16 20 16 20
  -- , onBackPressed push $ const BackPressed
  , afterRender push (const AfterRender)
  ][ zooTicketView state push
  ,  carouselDotView state push
  ]

zooTicketView :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
zooTicketView state push =
  let activeItem = state.props.activeListItem
  in
  PrestoAnim.animationSet
  [ translateInXForwardAnim true , translateInXForwardAnim true] $
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , background $ getTicketBackgroundColor activeItem.ticketServiceName
  , orientation VERTICAL
  , cornerRadius 8.0
  ][ linearLayout
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , background $ getTicketBackgroundColor activeItem.ticketServiceName
      , orientation VERTICAL
      , id (getNewIDWithTag "QR_TICKET")
      , padding $ Padding 16 24 16 0
      , cornerRadius 8.0
      ][ ticketHeaderView state push (getPlaceColor activeItem.ticketServiceName) (getInfoColor activeItem.ticketServiceName)
      ,  ticketImageView state push
      ,  bookingInfoView state push
      ]
  ,  shareTicketView state push
  ]


getTicketBackgroundColor :: String -> String
getTicketBackgroundColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.black900
  "Videography" -> Color.yellow800
  "Aquarium" -> "#DFE8FF"
  _ -> Color.black900

getShareButtonIcon :: String -> String
getShareButtonIcon ticketServiceName = case ticketServiceName of
  "Entrance" -> "ny_ic_share_unfilled_white"
  "Videography" -> "ny_ic_share_unfilled_black"
  "Aquarium" -> "ny_ic_share_unfilled_black"
  _ -> "ny_ic_share_unfilled_white"

getShareButtonColor :: String -> String
getShareButtonColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.white900
  "Videography" -> Color.black900
  "Aquarium" -> Color.black900
  _ -> Color.white900

getPlaceColor :: String -> String
getPlaceColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.white900
  "Videography" -> Color.black800
  "Aquarium" -> Color.black800
  _ -> Color.white900

getInfoColor :: String -> String
getInfoColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.white900
  "Videography" -> Color.black900
  "Aquarium" -> Color.black900
  _ -> Color.white900

ticketHeaderView :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> String -> String -> PrestoDOM (Effect Unit) w
ticketHeaderView state push placeColor infoColor  =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width $ MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  ][ imageView
     [ margin $ Margin 0 0 10 0
     , width $ V 24
     , height $ V 24
     , imageWithFallback $ getTicketImage activeItem.ticketServiceName 
     ]
  ,  linearLayout
     [ width $ MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     ][ tvView state.data.selectedBookingInfo.ticketPlaceName placeColor (MarginBottom 0) (FontStyle.body3 TypoGraphy)
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][ tvView (convertUTCtoISC state.data.selectedBookingInfo.visitDate "Do MMM, YYYY") infoColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
        ,  dotView placeColor (Margin 10 10 10 10) 5
        ,  tvView ("Total : ₹ " <>  show state.data.selectedBookingInfo.amount) infoColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)
        ]
     ]
  
  ]

getTicketImage :: String -> String
getTicketImage ticketServiceName = case ticketServiceName of
  "Entrance" -> fetchImage FF_ASSET "ny_ic_ticket"
  "Videography" -> fetchImage FF_ASSET "ny_ic_ticket_black"
  "Aquarium" -> fetchImage FF_ASSET "ny_ic_ticket_black"
  _ -> fetchImage FF_ASSET "ny_ic_ticket"

carouselDotView :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
carouselDotView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop 12
  ] (DA.mapWithIndex (\index item -> if index == state.props.activeIndex then (dotView Color.black900 (Margin 2 2 2 2) 6) else (dotView Color.grey900 (Margin 2 2 2 2) 6) ) state.data.selectedBookingInfo.services)

ticketImageView :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketImageView state push =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , margin $ MarginVertical 16 16
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      ][  linearLayout
          [ weight 1.0
          , height $ V 150
          , gravity CENTER
          , onClick push $ const DecrementSliderIndex
          , clickable $ if state.props.leftButtonDisable then false else true
          ][ imageView
              [ width $ V 24
              , height $ V 24
              , imageWithFallback $ getLeftButtonForSlider state.props.activeListItem.ticketServiceName state.props.leftButtonDisable
              , visibility $ if state.props.leftButtonDisable then INVISIBLE else VISIBLE
              ]
          ]
        , linearLayout
          [ weight 1.0
          , width $ V 192
          , height $ V 192
          , gravity CENTER
          ][ imageView
            [ width $ V 194
            , height $ V 194
            , id $ spy "QRID" (getNewIDWithTag "ticketQRView")
            , background Color.black900
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
            , afterRender push (const (TicketQRRendered (getNewIDWithTag "ticketQRView") activeItem.ticketServiceShortId ))
            ]
          ]
        , linearLayout
          [ weight 1.0
          , height $ V 150
          , gravity CENTER
          , onClick push $ const IncrementSliderIndex
          , clickable $ if state.props.rightButtonDisable then false else true
          ][ imageView
            [ width $ V 24
            , height $ V 24
            , imageWithFallback $ getRightButtonForSlider state.props.activeListItem.ticketServiceName state.props.rightButtonDisable
            , visibility $ if state.props.rightButtonDisable then INVISIBLE else VISIBLE
            ]
          ]
      ]
    , tvView ((getTextForQRType activeItem.ticketServiceName) <> "QR") (getInfoColor activeItem.ticketServiceName) (MarginVertical 10 10) (FontStyle.subHeading1 TypoGraphy)
    , pillView state push (getPillBackgroundColor activeItem.ticketServiceName) (getPillInfoColor activeItem.ticketServiceName)
  ]

getTextForQRType :: String -> String
getTextForQRType ticketServiceName = case ticketServiceName of
  "Entrance" -> "Zoo Entry "
  "Videography" -> "Photo / VideoGraphy "
  "Aquarium" -> "Aquarium Entry "
  _ -> ticketServiceName <> " "

getPillBackgroundColor :: String -> String
getPillBackgroundColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.black6000
  "Videography" -> Color.yellow900
  "Aquarium" ->  Color.blue800
  _ -> Color.black6000

getPillInfoColor :: String -> String
getPillInfoColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.grey900
  "Videography" -> Color.black800
  "Aquarium" ->  Color.white900
  _ -> Color.grey900
  
getLeftButtonForSlider :: String -> Boolean -> String
getLeftButtonForSlider ticketServiceName buttonDisabled = case ticketServiceName of
  "Videography" -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_left"
  "Aquarium" -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_left"
  _ -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_left_white"

getRightButtonForSlider :: String -> Boolean -> String
getRightButtonForSlider ticketServiceName buttonDisabled = case ticketServiceName of
  "Videography" -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_right"
  "Aquarium" ->if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_right"
  _ -> if buttonDisabled then "" else fetchImage FF_ASSET "ny_ic_chevron_right_white"

pillView :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> String -> String -> PrestoDOM (Effect Unit) w
pillView state push backgroudColor textColor =
  let activeItem = state.props.activeListItem
      peopleCatInfo = getPCs activeItem.categories
  in
  horizontalScrollView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , padding $ Padding 10 10 10 10
  ][ linearLayout
     [ width WRAP_CONTENT
     , height MATCH_PARENT
     , orientation HORIZONTAL
     ] $ [ linearLayout
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        ] (map (\item -> (individualPill state push backgroudColor textColor (show item.numberOfUnits) (getNameAccToPlaceType item.name activeItem) )) peopleCatInfo.pcs)
     ] <> (case activeItem.slot of 
           Nothing -> []
           Just slot -> case convertUTCToISTAnd12HourFormat slot of
            Nothing -> []
            Just formattedSlot -> [individualPill state push backgroudColor textColor formattedSlot ""])
  ]
   where
    getPCs :: Array ST.TicketBookingCategoryDetails -> {length :: Int, pcs :: Array ST.TicketBookingPeopleCategoryDetails}
    getPCs categories = case categories DA.!! 0 of
      Nothing -> { length : 0, pcs : []}
      Just cat -> {length : DA.length cat.peopleCategories, pcs : cat.peopleCategories}

    getNameAccToPlaceType pcName activeItem = pcName -- todo : handle the case to show person for ferry

individualPill :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> String -> String -> String -> String -> PrestoDOM (Effect Unit) w
individualPill state push backgroudColor textColor title desc =
  linearLayout
  [ width $ WRAP_CONTENT
  , height $ WRAP_CONTENT
  , orientation HORIZONTAL
  , background backgroudColor
  , cornerRadius 30.0
  , padding $ Padding 16 6 16 6
  , margin $ MarginLeft 10
  ] [  linearLayout
      [ width $ WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ][  tvView (title <> " " <> desc) textColor (MarginBottom 0) (FontStyle.subHeading1 TypoGraphy)]
  ]

dotView :: forall w. String -> Margin -> Int -> PrestoDOM (Effect Unit) w
dotView color layMargin size =
  linearLayout
  [ width $ V size
  , height $ V size
  , background color
  , cornerRadius 30.0
  , margin $ layMargin
  ][]

tvView :: forall w. String -> String -> Margin -> (forall properties. (Array (Prop properties))) -> PrestoDOM (Effect Unit) w
tvView textString textColor textMargin fontSt = 
  textView
  ([ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text textString
  , color textColor
  , margin textMargin
  , gravity CENTER
  ] <>  fontSt )

bookingInfoView :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bookingInfoView state push =
  let activeItem = state.props.activeListItem
      validityTime = (maybe (convertUTCtoISC (fromMaybe "" activeItem.expiryDate) "hh:mm A") (\sl -> fromMaybe "" (convertUTCToISTAnd12HourFormat sl)) activeItem.slot )  
                     <> ", " <> (convertUTCtoISC (fromMaybe "" activeItem.expiryDate) "Do MMM YYYY")
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ]([  bookingInfoListItemView push state "Service ID" activeItem.ticketServiceShortId
    , separatorView (getSeparatorColor activeItem.ticketServiceName)
    , bookingInfoListItemView push state (getTextForQRType activeItem.ticketServiceName) ("₹" <> show activeItem.amount)
  ] <> case activeItem.expiryDate of 
          Just expiryDate ->  [ separatorView (getSeparatorColor activeItem.ticketServiceName)
                              , bookingInfoListItemView push state "Valid Until" validityTime]
          _ -> [])

getSeparatorColor :: String -> String
getSeparatorColor ticketServiceName = case ticketServiceName of
  "Entrance" -> Color.black700
  "Videography" -> Color.white900
  "Aquarium" -> Color.white900
  _ -> Color.black700

bookingInfoListItemView :: forall w.   (Action -> Effect Unit) -> ST.TicketInfoScreenState -> String -> String -> PrestoDOM (Effect Unit ) w
bookingInfoListItemView push state key value =
  let activeItem = state.props.activeListItem
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginVertical 12 12
  ][  textView
      ([ weight 1.0
      , height WRAP_CONTENT
      , text key
      , color $ getPlaceColor activeItem.ticketServiceName
      ] <> FontStyle.body1 TypoGraphy)
    , textView
      ([ weight 1.0
      , height WRAP_CONTENT
      , text value
      , onClick push $ const $ if key == "Service ID" then Copy value else NoAction
      , color $ getPlaceColor activeItem.ticketServiceName
      , gravity RIGHT
      ] <> FontStyle.body1 TypoGraphy)
  ]

shareTicketView :: forall w. ST.TicketInfoScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
shareTicketView state push =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop 16
  , padding $ Padding 16 0 16 24
  , onClick push $ const $ ShareTicketQR state.props.activeListItem.ticketServiceName
  ][imageView
    [ height $ V 16
    , width $ V 16
    , imageWithFallback $ fetchImage FF_ASSET $ getShareButtonIcon state.props.activeListItem.ticketServiceName
    , margin $ MarginRight 8
    ]
  , textView $ 
    [ height $ WRAP_CONTENT
    , width $ WRAP_CONTENT
    , padding $ PaddingBottom 5
    , textFromHtml $ "<u>" <> "Share" <> "</u>"
    , color $ getShareButtonColor state.props.activeListItem.ticketServiceName
    ] <> FontStyle.body1 TypoGraphy
  ]