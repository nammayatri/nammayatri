{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.QuoteListModel.Controller where

import Components.PrimaryButton as PrimaryButtonController
import Components.QuoteListItem as QuoteListItemController
import Data.Maybe (Maybe)
import Screens.Types (TipViewProps)
import MerchantConfig.Types (AppConfig)

data Action
  = GoBack
  | NoAction
  | PrimaryButtonActionController PrimaryButtonController.Action
  | QuoteListItemActionController QuoteListItemController.Action
  | HomeButtonActionController PrimaryButtonController.Action
  | TryAgainButtonActionController PrimaryButtonController.Action
  | CancelAutoAssigning
  | TipBtnClick Int Int
  | TipViewPrimaryButtonClick PrimaryButtonController.Action
  | HidePopUp

type QuoteListModelState
  = { source :: String
    , destination :: String
    , quoteListModel :: Array QuoteListItemController.QuoteListItemState
    , selectedQuote :: Maybe String
    , autoSelecting :: Boolean
    , searchExpire :: Int
    , showProgress :: Boolean
    , tipViewProps :: TipViewProps
    , findingRidesAgain :: Boolean
    , progress :: Number
    , appConfig :: AppConfig
    , vehicleVariant :: String
    }
