module Components.DriverProfileScoreCard where

import Prelude
import PrestoDOM
import PrestoDOM
import Prelude hiding(zero)
import Font.Style as FontStyle
import Styles.Colors as Colors
import Engineering.Helpers.Commons
import Components.PrimaryButton as PrimaryButton
import Effect
import Data.Maybe
import Common.Types.App
import Helpers.Utils
import Animation
import PrestoDOM.Animation as PrestoAnim
import Services.API
import Data.Array as DA
import Data.Int
import Resource.Localizable.StringsV2
import Resource.Localizable.TypesV2
import Language.Types
import Language.Strings (getString)
import Data.Function.Uncurried
import RemoteConfig as RC
import Storage
import Data.Show.Generic (genericShow)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.String (replaceAll, Pattern(..), Replacement(..))


data ScoreCardType = ExtraCharge | Cancellation | Safety

derive instance genericScoreCardType :: Generic ScoreCardType _
instance showScoreCardType :: Show ScoreCardType where show = genericShow
instance eqScoreCardType :: Eq ScoreCardType where eq = genericEq

type DriverProfileScoreCardType = {
    background :: String
,   badgeTitle :: String
,   title :: String
,   titleColor :: String
,   image :: String
,   score :: String
,   description :: String
,   primaryButtonText :: String
,   type :: ScoreCardType
,   imageWidth :: Length
,   imageHeight :: Length
}

data Action = OnPrimaryButtonClick ScoreCardType PrimaryButton.Action


verticalView :: forall w. (Action -> Effect Unit) -> DriverProfileScoreCardType -> PrestoDOM (Effect Unit) w
verticalView push config =
    linearLayout[
        width $ V $ (screenWidth unit) / 2 - 22
    ,   orientation VERTICAL
    ,   margin $ Margin 0 16 12 16
    ,   gravity CENTER
    ][
        textView $ [
            text config.badgeTitle
        ,   color Colors.black
        ,   gravity CENTER
        ,   margin $ MarginBottom 12
        ] <> (FontStyle.subHeading2 TypoGraphy)
    ,   linearLayout [
            width $ V $ (screenWidth unit) / 2 - 22
        ,   height $ WRAP_CONTENT
        ,   orientation VERTICAL
        ,   background Colors.aliceBlueLight
        ,   cornerRadius 16.0
        ,   gravity CENTER
        ][
            linearLayout [
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ,   orientation VERTICAL
            ,   background config.background
            ,   cornerRadius 16.0
            ,   padding $ PaddingVertical 12 12
            ,   gravity CENTER
            ][
                imageView [
                    width config.imageWidth
                ,   height config.imageHeight
                ,   imageWithFallback config.image
                ]
            ,   textView $ [
                    text config.title
                ,   color config.titleColor
                ,   margin $ MarginTop 16
                ] <> (FontStyle.h2 TypoGraphy)
            ]
        ,   linearLayout [
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ,   orientation VERTICAL
            ,   padding $ Padding 12 12 12 12
            ,   gravity CENTER
            ][
                textView $ [
                    text config.score
                ,   color Colors.black
                ,   gravity CENTER
                ] <> (FontStyle.h3 TypoGraphy)
            ,   textView $ [
                    text config.description
                ,   color Colors.black800
                ,   margin $ MarginTop 4
                ,   gravity CENTER
                ] <> (FontStyle.body3 TypoGraphy)
            ,   PrimaryButton.view (push <<< OnPrimaryButtonClick config.type) (getPrimaryButtonConfig config.primaryButtonText)
            ]
        ]
    ]


getPrimaryButtonConfig :: String -> PrimaryButton.Config
getPrimaryButtonConfig text = PrimaryButton.config{
    textConfig  {
        text = text
    ,   color = Colors.blue800
    ,   textStyle = FontStyle.Tags
    }
,   background = "#142194FF"
,   width = MATCH_PARENT
,   margin = MarginTop 12
}


horizontalView :: forall w. (Action -> Effect Unit) -> DriverProfileScoreCardType -> PrestoDOM (Effect Unit) w
horizontalView push config =
    linearLayout[
        width MATCH_PARENT
    ,   height WRAP_CONTENT
    ,   orientation VERTICAL
    ,   gravity CENTER
    ,   margin $ MarginVertical 16 16
    ,   onClick push $ const (OnPrimaryButtonClick config.type PrimaryButton.OnClick)
    ][
        textView $ [
            text config.badgeTitle
        ,   color Colors.black
        ,   gravity CENTER
        ,   margin $ MarginBottom 12
        ] <> (FontStyle.subHeading2 TypoGraphy)
    ,  linearLayout[
            width MATCH_PARENT
        ,   height WRAP_CONTENT
        ,   orientation VERTICAL
        ,   background Colors.aliceBlueLight
        ,   cornerRadius 16.0
        ,   padding $ Padding 16 16 16 16
        ][
            linearLayout[
                width MATCH_PARENT
            ,   height WRAP_CONTENT
            ][
                linearLayout[height WRAP_CONTENT, weight 1.0][]
            ,   linearLayout[
                    width WRAP_CONTENT
                ,   height WRAP_CONTENT
                ,   orientation VERTICAL
                ,   gravity CENTER
                ,   margin $ MarginRight 16
                ][
                    textView $ [
                        text config.title
                    ,   cornerRadius 40.0
                    ,   color Colors.white900
                    ,   gravity CENTER
                    ,   background config.titleColor
                    ,   padding $ Padding 16 4 16 4
                    ] <> (FontStyle.h1 TypoGraphy)
                ,   textView $ [
                        text config.score
                    ,   color config.titleColor
                    ,   width MATCH_PARENT
                    ,   margin $ MarginTop 8
                    ,   gravity CENTER
                    ] <> (FontStyle.body8 TypoGraphy)
                ]
            , linearLayout[height WRAP_CONTENT, weight 1.0][]
            , imageView[
                    width $ V 80
                ,   height $ V 68
                ,   imageWithFallback config.image
                ,   margin $ MarginLeft 16
                ]
            , linearLayout[height WRAP_CONTENT, weight 1.0][]
            ]
        ,   textView $ [
                text $ replaceAll (Pattern "\n") (Replacement " ") config.description
            ,   width MATCH_PARENT
            ,   height WRAP_CONTENT
            ,   color Colors.black800
            ,   gravity CENTER
            ,   background config.background
            ,   margin $ MarginTop 16
            ,   cornerRadius 8.0
            ,   padding $ Padding 16 12 16 12
            ] <> (FontStyle.paragraphText TypoGraphy)
        ]
    ]
