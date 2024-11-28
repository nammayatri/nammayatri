{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.MetroTicketBooking.ComponentConfig where

import Prelude
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton 
import Components.PrimaryEditText as PrimaryEditText
import PrestoDOM
import Styles.Colors as Color
import Helpers.Utils (convertTo12HourFormat , fetchImage, FetchImageFrom(..))
import Prelude ((<>))
import Common.Types.App(LazyCheck(..), Price)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Utils (getCityFromString)
import Data.Maybe
import Font.Style as FontStyle
import JBridge as JB
import Screens.Types as ST
import Components.RequestInfoCard as InfoCard
import Language.Strings 
import Resources.LocalizableV2.Strings (getEN)
import Language.Types
import Helpers.Utils (CityMetroConfig(..))
import Components.Banner as Banner
import DecodeUtil (getAnyFromWindow)
import Data.Function.Uncurried (runFn3)
import MerchantConfig.Types (MetroConfig)
import Storage
import Services.API (FRFSConfigAPIRes(..),TicketServiceType(..), FRFSDiscountReq(..))
import Mobility.Prelude (getNumberWithSuffix)
import Debug
import Data.Array as DA
import Accessor (_amount)
import Data.Lens((^.))
import Data.Int as INT

metroTicketBookingHeaderConfig :: ST.MetroTicketBookingScreenState -> GenericHeader.Config
metroTicketBookingHeaderConfig state = let
    config = GenericHeader.config
    genericHeaderConfig' = config 
        {
          height = WRAP_CONTENT
        , width = WRAP_CONTENT
        , prefixImageConfig {
           visibility = VISIBLE
          , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
          , height = V 25
          , width = V 25
          , margin = Margin 16 16 16 16
          } 
        , padding = PaddingVertical 5 5
        , textConfig {
            text = if state.props.currentStage == ST.OfferSelection then "Offers" else if state.props.ticketServiceType == BUS then getString BUY_BUS_TICKETS else getString BUY_METRO_TICKETS
          , color = Color.darkCharcoal
          }
        , suffixImageConfig {
            visibility = GONE
          }
        }
    in genericHeaderConfig'

updateButtonConfig :: ST.MetroTicketBookingScreenState -> PrimaryButton.Config
updateButtonConfig state = let
    city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
    config = PrimaryButton.config
    price = state.data.ticketPrice * (INT.toNumber state.data.ticketCount)
    eventDiscountAmount = fromMaybe 0 state.data.eventDiscountAmount
    (FRFSConfigAPIRes metroBookingConfigResp) = state.data.metroBookingConfigResp
    priceWithoutDiscount = spy "priceWithoutDiscount" (((metroBookingConfigResp.discount * price) / 100.0) + price)
    discountText = if price /= priceAfterExtraDiscount then ("&nbsp;&nbsp; " <> " ₹" <> "<strike> " <> "<span style='color:#7F6A34;'>"<> (show price)  <> " </span>" <> " </strike>" <> " ") else ""
    cashbackText = if eventDiscountAmount > 0 then (" (" <> "₹" <> show eventDiscountAmount <> " cashback)") else ""
    updateButtonConfig' = config 
        { textConfig { textFromHtml = Just $ if (state.props.currentStage /= ST.MetroTicketSelection && state.props.currentStage /= ST.BusTicketSelection ) then (getString BOOK_AND_PAY <> discountText <> " ₹" <> (show priceAfterExtraDiscount) <> cashbackText) else (getString GET_FARE)}
        , height = (V 48)
        , cornerRadius = 8.0
        , margin = (Margin 16 0 16 0)
        , id = "PrimaryButtonUpdate"
        , enableLoader = (JB.getBtnLoader "PrimaryButtonUpdate")
        , isClickable = state.props.termsAndConditionsSelected && state.props.isButtonActive
        , alpha = if (state.props.termsAndConditionsSelected && state.props.isButtonActive) then 1.0 else 0.5
        }
    in updateButtonConfig'
    where
      appliedDiscountCodes :: Array String
      appliedDiscountCodes = maybe [] extractDiscountCodes state.data.applyDiscounts

      extractDiscountCodes :: Array FRFSDiscountReq -> Array String
      extractDiscountCodes discounts = map (\(FRFSDiscountReq discountItem) -> discountItem.code) discounts

      priceAfterExtraDiscount = 
        state.data.ticketPrice * (INT.toNumber state.data.ticketCount) - ( 
            case DA.find (\discount -> discount.code == (fromMaybe "" $ DA.head appliedDiscountCodes)) state.data.discounts of
              Just discount -> discount.price.amount
              _ -> 0.0)

metroBannerConfig :: CityMetroConfig -> ST.MetroTicketBookingScreenState -> Banner.Config
metroBannerConfig (CityMetroConfig cityMetroConfig) state =
  let
    config = Banner.config
    appName = fromMaybe state.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
    (FRFSConfigAPIRes metroBookingConfigResp) = state.data.metroBookingConfigResp
    title' = if (metroBookingConfigResp.isEventOngoing == Just true) then (getString $ METRO_FREE_TICKET_EVENT $ getNumberWithSuffix $ fromMaybe 0 metroBookingConfigResp.freeTicketInterval) else getString $ EXPERIENCE_HASSLE_FREE_METRO_BOOKING appName
    actionText' = config.actionText { 
                      text = if (metroBookingConfigResp.isEventOngoing == Just true) then (getString $ METRO_FREE_TICKET_EVENT_DESC (getNumberWithSuffix $ fromMaybe 0 metroBookingConfigResp.freeTicketInterval) (maybe "" show metroBookingConfigResp.maxFreeTicketCashback)) else ""
                    , textColor = Color.metroBlue
                    , style = FontStyle.Tags
                  }
    imageUrl' = if (metroBookingConfigResp.isEventOngoing == Just true) then fetchImage FF_ASSET "ny_ic_metro_offer" else fetchImage FF_COMMON_ASSET cityMetroConfig.bannerImage
    config' = config
      {
        backgroundColor = cityMetroConfig.bannerBackgroundColor
      , stroke = "1," <> Color.grey900
      , imageHeight = V 84
      , imageWidth = V 124
      , margin = MarginVertical 12 12
      , imagePadding = PaddingVertical 0 0
      , title = title'
      , titleColor = cityMetroConfig.bannerTextColor
      , padding = PaddingLeft 5
      , actionTextVisibility = metroBookingConfigResp.isEventOngoing == Just true 
      , cornerRadius = 8.0
      , imageUrl = imageUrl'
      , imageMargin = Margin 18 6 6 6
      , actionText = actionText'
      , actionPadding = PaddingHorizontal 0 0
      }
  in config'

metroTimeErrorPopupConfig :: ST.MetroTicketBookingScreenState -> CityMetroConfig -> InfoCard.Config
metroTimeErrorPopupConfig state (CityMetroConfig cityMetroConfig) = let
  requestInfoCardConfig' =  InfoCard.config{
    title {
      text = getString METRO_BOOKING_TIMINGS, 
      accessibilityHint = "Metro Booking Timings"
    }
  , primaryText {
      text = cityMetroConfig.errorPopupTitle ,
      padding = Padding 16 16 0 0,
      textStyle = FontStyle.ParagraphText,
      color = Color.black700,
      accessibilityHint = cityMetroConfig.errorPopupTitle
    }
  , secondaryText {
      text = getString PLEASE_COME_BACK_LATER_METRO,
      visibility = VISIBLE,
      padding = PaddingLeft 16,
      color = Color.black700,
      textStyle = FontStyle.ParagraphText,
      width = V $ JB.getWidthFromPercent 100,
      accessibilityHint = "Please come back later during the eligible time to purchase tickets."
    }
  , imageConfig {
      imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_red_warning_with_red_circle_bg",
      height = V 130,
      width = V 130,
      padding = Padding 0 2 2 0,
      visibility = VISIBLE
    }
  , buttonConfig {
      text = getString GOT_IT,
      padding = PaddingVertical 16 20,
      accessibilityHint = (getEN GOT_IT) <> " : Button"
    }
  }
  in requestInfoCardConfig'