{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.IssueView.Controller where
import Font.Style as FontStyle
import Font.Size as FontSize
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), height, width)
import Styles.Colors as Color
import Common.Types.App(LazyCheck(..))

type IssueState = {
    firstTextConfig :: TextConfig ,
    secondTextConfig :: TextConfig ,
    thirdTextConfig :: TextConfig ,
    fourthTextConfig :: TextConfig ,
    fifthTextConfig :: TextConfig ,
    issue :: IssueInfo 
}

data Action = Remove String
            | CallSupportCenter
            | IssueClick IssueInfo

type TextConfig = 
  {   text :: String 
    , fontSize :: Int
    , focusIndex :: Int
    , fontStyle :: String
    , gravity :: Gravity
    , visibility :: Visibility
    , color :: String
    , height :: Length
    , width :: Length
    , cornerRadius :: Number
    , padding :: Padding
    , margin :: Margin
    , weight :: Number
    , alpha :: Number
  }

type IssueInfo = {
    issueReportId :: String,
    status :: String,
    category :: String,
    createdAt :: String
}

config :: IssueState
config = {
    firstTextConfig : {
       text : ("Hello1")
    , fontSize : FontSize.a_13
    , focusIndex : 0
    , fontStyle : FontStyle.bold LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.black900
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 1.0
    , alpha : 0.9
    },
      secondTextConfig : {
        text : ("Hello2")
    , fontSize :FontSize.a_13
    , focusIndex : 0
    , fontStyle : FontStyle.medium LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.black700
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 6 0)
    , weight : 1.0
    , alpha : 0.9
    },
      thirdTextConfig : {
        text : ("Issue Number")
    , fontSize : FontSize.a_13
    , focusIndex : 0
    , fontStyle : FontStyle.medium LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.black800
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 1.0
    , alpha : 0.9
      },
      fourthTextConfig : {
        text :("REMOVE_ISSUE")
    , fontSize : FontSize.a_13
    , focusIndex : 0
    , fontStyle : FontStyle.medium LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.blue900
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 5 0 0)
    , weight : 1.0
    , alpha : 0.9
    },
      fifthTextConfig : {
        text :("CALL_SUPPORT_NUMBER")
    , fontSize : FontSize.a_13
    , focusIndex : 0
    , fontStyle : FontStyle.medium LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.blue900
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 35 0 0 0)
    , weight : 1.0
    , alpha : 0.9
    },
    issue :  { issueReportId: "12345", status: "RESOLVED", category: "LOST_AND_FOUND", createdAt: "2023-07-31 12:34:56"}
}

