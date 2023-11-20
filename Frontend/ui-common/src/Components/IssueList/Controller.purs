{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.IssueList.Controller where

import Font.Size as FontSize
import Language.Types(STR(..))
import Font.Style as FontStyle
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), height, width)
import Styles.Colors as Color
import Common.Types.App(LazyCheck(..))
import Screens.Types (IssueInfo(..),IssueModalType(..))
import Components.IssueView.Controller as IssueViewController

data Action =  BackPressed 
              | AfterRender 
              | IssueViewAction IssueViewController.Action

type IssueListFlowState = {
  headerConfig :: HeadConfig ,
  issueViewConfig :: IssueViewController.IssueState ,
  issues :: Array IssueInfo,
  issueListTypeModal :: IssueModalType
}

type HeadConfig = {
    padding :: Padding,
    imageConfig :: TextConfig ,
    headTextConfig :: TextConfig
}

type TextConfig = {
      text :: String 
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


config :: IssueListFlowState
config = {
    headerConfig : {
      padding : Padding 5 16 5 16,
      imageConfig : {
        text : ""
      , fontSize : FontSize.a_14
      , focusIndex : 0
      , fontStyle : FontStyle.semiBold LanguageStyle
      , gravity : CENTER
      , visibility : VISIBLE
      , color : Color.red
      , height : V 30
      , width : V 30
      , cornerRadius : 0.0
      , padding : Padding 2 2 2 2
      , margin : MarginLeft 5
      , weight : 1.0
      , alpha : 0.0
      },
      headTextConfig : {
        text : ""
      , fontSize : FontSize.a_18
      , focusIndex : 0
      , fontStyle : FontStyle.semiBold LanguageStyle
      , gravity : CENTER
      , visibility : VISIBLE
      , color : Color.black900
      , height : WRAP_CONTENT
      , width : WRAP_CONTENT
      , cornerRadius : 0.0
      , padding : Padding 0 0 0 0
      , margin : MarginLeft 20
      , weight : 1.0
      , alpha : 0.0
      } },
    issueViewConfig : IssueViewController.config,
    issues : [],
    issueListTypeModal : HELP_AND_SUPPORT_SCREEN_MODAL     
}
