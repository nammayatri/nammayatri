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
import Prelude ((==), (<>), ($))
import Screens.Types as ST

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