{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.IssueView.View where
import Components.IssueView.Controller (Action(..), IssueState, IssueInfo, TextConfig)
import Effect (Effect)
import Styles.Colors as Color
import Font.Style as FontStyle
import Common.Types.App(LazyCheck(..))
import Prelude (Unit, const, ($), (/), (==), (<>),(<))
import Data.String (take)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, alpha, cornerRadius, id, visibility, stroke)

view :: ( Action ->Effect Unit ) -> IssueState -> forall w . PrestoDOM (Effect Unit) w
view push state = linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , visibility  VISIBLE
            , stroke $ "1," <> Color.grey900
            , cornerRadius 10.0
            , margin (Margin 15 0 15 20)
            , onClick push $ const (IssueClick state.issue)
            ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , gravity CENTER_VERTICAL
              , padding (Padding 16 20 16 10)
              , visibility  VISIBLE 
              ][
                 linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , gravity CENTER_VERTICAL
                  ][ renderText state.issue.category  state.firstTextConfig
                    , linearLayout
                    [  height WRAP_CONTENT
                     , weight 1.0
                     ][]
                     , renderText state.issue.createdAt state.secondTextConfig
                  ]
                  ,linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , gravity CENTER_VERTICAL
                  , margin (Margin 0 13 0 0)
                  ][ issueId state
                    , linearLayout
                    [  height WRAP_CONTENT
                     , weight 1.0
                     ][]
                    , renderUpdated state.issue
                  ]
                  , linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , gravity CENTER_VERTICAL
                  , margin $ Margin 0 13 0 0
                  ][
                   removeButton push state
                  ,callSupportCenterButton push state
                  ]
              ]
            ]

renderUpdated :: IssueInfo -> forall w . PrestoDOM (Effect Unit) w
renderUpdated state = linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , background Color.blue900
                    , orientation HORIZONTAL
                    , cornerRadius 12.0
                    , padding (Padding 12 2 12 2)
                    , gravity CENTER_VERTICAL
                    , visibility (if (state.status  == "AWAIT") then VISIBLE else GONE)
                    ][ textView $
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , gravity CENTER
                        , text "Updated"
                        , color Color.white900
                        ] <> FontStyle.paragraphText TypoGraphy
                    ]

renderText :: String -> TextConfig -> forall w . PrestoDOM (Effect Unit) w
renderText content config = textView
                    [ height config.height   
                    , width config.width 
                    , text content
                    , visibility  config.visibility  
                    , color config.color 
                    , fontStyle $ config.fontStyle
                    , textSize config.fontSize 
                    , margin config.margin                              
                    , alpha config.alpha             
                    ]

issueId :: IssueState -> forall w . PrestoDOM (Effect Unit) w
issueId state = textView
                    [ height state.thirdTextConfig.height   
                    , width state.thirdTextConfig.width 
                    , text (state.thirdTextConfig.text <> " " <> (take 13 state.issue.issueReportId))
                    , visibility  state.thirdTextConfig.visibility  
                    , color state.thirdTextConfig.color 
                    , fontStyle $ state.thirdTextConfig.fontStyle
                    , textSize state.thirdTextConfig.fontSize 
                    , margin state.thirdTextConfig.margin                              
                    , alpha state.thirdTextConfig.alpha             
                    ]

removeButton :: ( Action ->Effect Unit ) -> IssueState -> forall w . PrestoDOM (Effect Unit) w
removeButton push state = textView
                  [ height state.fourthTextConfig.height              
                  , width state.fourthTextConfig.width    
                  , text state.fourthTextConfig.text 
                  , color state.fourthTextConfig.color 
                  , visibility  state.fourthTextConfig.visibility 
                  , color state.fourthTextConfig.color   
                  , fontStyle $ state.fourthTextConfig.fontStyle
                  , textSize state.fourthTextConfig.fontSize
                  , alpha state.fourthTextConfig.alpha
                  , onClick push (const (Remove state.issue.issueReportId))
                  ]

callSupportCenterButton :: ( Action ->Effect Unit ) -> IssueState -> forall w . PrestoDOM (Effect Unit) w
callSupportCenterButton push state = textView
                  [ height WRAP_CONTENT             
                  , width state.fifthTextConfig.width    
                  , text state.fifthTextConfig.text 
                  , color state.fifthTextConfig.color 
                  , visibility  state.fifthTextConfig.visibility 
                  , margin $ state.fifthTextConfig.margin
                  , color state.fifthTextConfig.color   
                  , fontStyle $ state.fifthTextConfig.fontStyle
                  , textSize state.fifthTextConfig.fontSize
                  , alpha state.fifthTextConfig.alpha
                  , onClick push (const (CallSupportCenter))
                  ]
