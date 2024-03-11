{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.ComponentConfig where



import Components.IssueList as IssueList
import Data.Array (length)
import Helpers.Utils (toStringJSON)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Prelude ((==), (<>), ($), (/), (-), unit, (||), not)
import Screens.Types as ST
import Components.PopUpModal as PopUpModal
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), Accessiblity(..), cornerRadius, padding, gravity)
import Engineering.Helpers.Commons as EHC
import Styles.Colors as Color
import PrestoDOM.Types.DomAttributes as PTD
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Font.Style (Style(..))

issueListState :: ST.HelpAndSupportScreenState -> IssueList.IssueListFlowState
issueListState state = let
      config' = IssueList.config
      inAppModalConfig' = config'{
        issues = if state.data.issueListType == ST.ONGOING_ISSUES_MODAL then state.data.ongoingIssueList else state.data.resolvedIssueList,
        issueListTypeModal = state.data.issueListType ,
        headerConfig {
          headTextConfig {
            text = if state.data.issueListType == ST.ONGOING_ISSUES_MODAL then ( getString ONGOING_ISSUE) <>(" : ") <> (toStringJSON (length state.data.ongoingIssueList)) else getString RESOLVED_ISSUE
          }
        },
        issueViewConfig {
          thirdTextConfig {
            text = getString ISSUE_NO
          },
          fourthTextConfig {
            visibility = boolToVisibility $ state.data.issueListType == ST.ONGOING_ISSUES_MODAL ,
            text = getString REMOVE_ISSUE
          },
          fifthTextConfig {
            visibility = boolToVisibility $ state.data.issueListType == ST.ONGOING_ISSUES_MODAL ,
            text = getString CALL_SUPPORT_NUMBER
          }
        }
      }
      in inAppModalConfig'

testRideConfirmationConfig :: ST.HelpAndSupportScreenState -> PopUpModal.Config
testRideConfirmationConfig state = let
  config' = PopUpModal.config
  popupType = state.props.popupType == ST.PROBLEM_WITH_TEST
  popUpConfig' = config'{
    gravity = CENTER,
    margin = MarginHorizontal 24 24 ,
    buttonLayoutMargin = Margin 16 24 16 20 ,
    topTitle {
      text = getString SEEMS_LIKE_THERE_IS_A_PROBLEM
    , visibility = boolToVisibility popupType
    , gravity = CENTER
    , margin = Margin 5 5 5 0
    },
    primaryText {
      text = case state.props.popupType of
                ST.TEST_RIDE_RECIEVED -> getString DID_YOU_RECEIVE_TEST_RIDE
                ST.PROBLEM_WITH_TEST -> getString PLEASE_TRY_THE_FOLLOWING_STEPS
                _ -> getString EVERYTHING_IS_OK
    , margin = Margin 16 19 16 0
    , gravity = if popupType then LEFT else CENTER
    , textStyle = if popupType then ParagraphText else Heading2},
    secondaryText {
      visibility = boolToVisibility $ not (state.props.popupType == ST.TEST_RIDE_RECIEVED)
    , text = case state.props.popupType of
                ST.PROBLEM_WITH_TEST -> "<span style='color:#2194FF'><u>"<> (getString CALL_OUR_SUPPORT_TEAM) <>"</u></span>"
                _ -> getString MOVE_TO_HIGH_DEMAND_AREA
    , margin = MarginTop 6
    , gravity = if popupType then LEFT else CENTER
    },
    option1 {
      text = getString NO
    , width = V $ ((EHC.screenWidth unit)-92)/2
    , isClickable = true
    , background = Color.white900
    , strokeColor = Color.black500
    , color = Color.black700
    , enableRipple = true
    , visibility = state.props.popupType == ST.TEST_RIDE_RECIEVED
    },
    option2 {
      text = if state.props.popupType == ST.TEST_RIDE_RECIEVED then getString YES else getString GOT_IT
    , margin = MarginLeft 12
    , width = V $ ((EHC.screenWidth unit)-92)/2
    , color = Color.yellow900
    , strokeColor = Color.black900
    , background = Color.black900
    , enableRipple = true
    },
    backgroundClickable = false,
    cornerRadius = PTD.Corners 15.0 true true true true,
    topTextVisibility = true,
    coverImageConfig {
      imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_dummy_ride_request"
    , visibility = boolToVisibility $ not (popupType)
    , margin = Margin 16 20 16 0
    , height = V 200
    , width = V 200
    }
  }
  in popUpConfig'