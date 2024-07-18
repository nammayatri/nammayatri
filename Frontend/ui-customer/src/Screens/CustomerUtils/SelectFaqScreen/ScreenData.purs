{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectFaqScreen.ScreenData where

import Screens.Types
import Common.Types.App 
import MerchantConfig.DefaultConfig as DC
import ConfigProvider
import Data.Maybe(Maybe(..))
import Foreign.Object (empty)
import MerchantConfig.Types (AppConfig)
import Foreign (Foreign)
import Foreign.Object (Object)

initData :: SelectFaqScreenState
initData = {
  data: {
    config : getAppConfig appConfig,
    logField : empty,
    issueList : [],
    issueListType : HELP_AND_SUPPORT_SCREEN_MODAL,
    categories : [],
    categoryName : ""
  },
  props:{
    apiFailure : false
  , needIssueListApiCall : true
  -- , isCallConfirmation : false
  }

}

-- ################################################  Types   ################################################

type SelectFaqScreenState =
  {
    data :: SelectFaqScreenData,
    props :: SelectFaqScreenProps
  }

type SelectFaqScreenData =
  {
    config :: AppConfig,
    issueList :: Array IssueInfo,
    issueListType :: IssueModalType,
    categories :: Array CategoryListType,
    categoryName :: String,
    logField :: Object Foreign
  }

type SelectFaqScreenProps =
  {
    apiFailure :: Boolean
  , needIssueListApiCall :: Boolean
  -- , isCallConfirmation :: Boolean

  }