{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.HelpAndSupportScreen.ScreenData where

import Screens.Types (HelpAndSupportScreenState)
import Prelude (class Eq, (<>))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Screens.Types as ST

initData :: HelpAndSupportScreenState
initData =
  { data:
      { categories: []
      , issueList: []
      , resolvedIssueList: []
      , ongoingIssueList: []
      , issueListType: ST.HELP_AND_SUPPORT_SCREEN_MODAL
      }
  , props: { isNoRides: false }
  }

data IssueOptions
  = OngoingIssues
  | ResolvedIssues
  | CallSupportCenter

derive instance genericIssueOptions :: Generic IssueOptions _

instance eqIssueOptions :: Eq IssueOptions where
  eq = genericEq

type IssueListType
  = { menuOptions :: IssueOptions }

otherIssueList :: Array IssueListType
otherIssueList =
  [ { menuOptions: OngoingIssues }
  , { menuOptions: ResolvedIssues }
  , { menuOptions: CallSupportCenter }
  ]
