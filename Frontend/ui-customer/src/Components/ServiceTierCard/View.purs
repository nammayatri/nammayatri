module Components.ServiceTierCard.View where

import Common.Types.App (LazyCheck(..))
import Components.StepsHeaderModel.Controller (Action(..))
import Data.Array (mapWithIndex,elem)
import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing)
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Commons as EHU
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..),fetchVehicleVariant)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude as MP
import Prelude (Unit, const, (<>), bind, ($), pure, unit, show, (==), (||), (&&), (/=), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), Visibility(..), Accessiblity(..), textFromHtml,PrestoDOM, alignParentBottom, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, stroke, text, textSize, textView, weight, width, imageWithFallback, id, afterRender, visibility, background, padding, accessibilityHint, accessibility, rippleColor, cornerRadius)
import Screens.Types (StepsHeaderModelState, FareProductType(..),VehicleVariant(..))
import Styles.Colors as Color
import Debug(spy)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (noFlags)
import Data.Either (either)
import Data.Maybe (maybe)
view :: forall w. Config -> PrestoDOM (Effect Unit) w
view config = let 
    showFPT = false --config.fareProductType == RENTAL
  in
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ Padding 4 4 8 4
    , background Color.blue600
    , gravity CENTER_VERTICAL
    , cornerRadius if EHC.os == "IOS" then 13.0 else 18.0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , background Color.blue800
        , padding $ Padding 4 bluePillPadding 5 bluePillPadding
        , visibility $ MP.boolToVisibility $ showACDetails config.name config.isAc config.fareProductType && config.showACPill
        , gravity CENTER_VERTICAL
        , cornerRadius if EHC.os == "IOS" then 11.0 else 18.0
        ]
        [ imageView
            [ height $ V 12
            , width $ V 12
            , imageWithFallback $ fetchImage FF_ASSET case config.vehicleVariant of
                                                      "AMBULANCE_VENTILATOR" ->  "ny_ic_ventilator_white"
                                                      _ -> "ny_ic_ac_white"
            , margin $ MarginRight 3
            ]
        , textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ getString AC
              , color Color.white900
              , padding $ PaddingBottom if EHC.os == "IOS" then 0 else 2
              , visibility $ MP.boolToVisibility $ config.vehicleVariant /= "AMBULANCE_VENTILATOR"
              ]
            <> FontStyle.tags TypoGraphy
        ]
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getServiceTierCardName $ if config.showACPill 
              then parseName config.name
              else config.name
          , color Color.black700
          , padding $ PaddingBottom if EHC.os == "IOS" then 0 else 2
          , margin $ MarginLeft 4
          ]
        <> FontStyle.tags TypoGraphy
    , case config.capacity of
        Just capacity ->
          linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , margin $ MarginLeft 4
            , accessibility ENABLE
            , visibility $ MP.boolToVisibility $ not (config.fareProductType == AMBULANCE)
            , accessibilityHint $ "Vehicle capacity : " <> show capacity
            ]
            [ textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text "•"
                , color Color.black700
                , padding $ PaddingBottom 3
                ] <> FontStyle.paragraphText TypoGraphy
            , textView $ 
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ parseFpt config.fareProductType
                , color Color.black700
                , margin $ MarginLeft 4
                , visibility $ MP.boolToVisibility showFPT
                ] <> FontStyle.tags TypoGraphy
            , textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text "•"
                , color Color.black700
                , visibility $ MP.boolToVisibility showFPT
                , padding $ PaddingBottom 2
                ] <> FontStyle.paragraphText TypoGraphy
            , imageView
                [ height $ V 12
                , width $ V 12
                , margin $ MarginLeft 3
                , padding $ PaddingBottom if EHC.os == "IOS" then 2 else 0
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_user_filled_dark"
                ]
            , textView $
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , padding $ PaddingBottom if EHC.os == "IOS" then 0 else 1
                , color Color.black700
                , text $ show capacity
                ] <> FontStyle.tags TypoGraphy
            ]
        Nothing -> linearLayout [] []
    ]
  where
    bluePillPadding = if EHC.os == "IOS" then 3 else 2
    getServiceTierCardName name = if name == "2 Wheeler" then "2W Parcel" else name


showACDetails :: String -> Maybe Boolean -> FareProductType -> Boolean
showACDetails name isAc fareProductType =
    case isAc of
        Just val -> val
        Nothing -> (not DS.contains (DS.Pattern "Non-AC") name) && Array.notElem name ["Auto", "Taxi", "AUTO_RICKSHAW", "Eco", "Bike Taxi","Non-AC ∙ O̶₂̶","Non-AC ∙ O₂"] && fareProductType /= DELIVERY

type Config
  = { name :: String
    , capacity :: Maybe Int
    , isAc :: Maybe Boolean
    , showACPill :: Boolean
    , fareProductType :: FareProductType
    , vehicleVariant :: String
    }

parseName :: String -> String
parseName name =
  let pattern = regex "\\bAC ∙ ?\\b" noFlags
  in if DS.contains (DS.Pattern "AC") name && not (DS.contains (DS.Pattern "Non-AC") name)
     then either (const name) (\r -> replace r "" name) pattern
     else name

parseFpt :: FareProductType -> String
parseFpt fpt = 
  case fpt of 
    RENTAL -> "Rental"
    INTER_CITY -> "InterCity" 
    ONE_WAY -> "Normal"
    ONE_WAY_SPECIAL_ZONE -> "Special Zone"
    DRIVER_OFFER -> "Normal"
    AMBULANCE -> "Ambulance"
    DELIVERY -> "Delivery"