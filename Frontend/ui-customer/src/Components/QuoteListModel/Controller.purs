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
import Screens.Types (TipViewProps, QuoteListItemState(..), City(..))
import MerchantConfig.Types (AppConfig)
import Components.TipsView as TipsView
import Components.ChooseVehicle.Controller as CVC
import Components.ProviderModel as PM

data Action = GoBack
            | NoAction
            | PrimaryButtonActionController PrimaryButtonController.Action
            | QuoteListItemActionController QuoteListItemController.Action
            | HomeButtonActionController PrimaryButtonController.Action
            | TryAgainButtonActionController PrimaryButtonController.Action
            | CancelAutoAssigning
            | TipViewPrimaryButtonClick PrimaryButtonController.Action
            | HidePopUp
            | ChangeTip
            | TipsViewActionController TipsView.Action
            | ProviderModelAC PM.Action
            | CancelTimer

type QuoteListModelState = {
     source :: String
  , destination :: String
  , quoteListModel :: Array QuoteListItemState
  , selectedQuote :: Maybe String
  , autoSelecting :: Boolean
  , searchExpire :: Int
  , showProgress :: Boolean
  , tipViewProps :: TipViewProps
  , findingRidesAgain :: Boolean
  , progress :: Number
  , appConfig :: AppConfig
  , vehicleVariant :: String
  , city :: City
  , customerTipArray :: Array String
  , customerTipArrayWithValues :: Array Int
  , providerSelectionStage :: Boolean
  , quoteList :: Array CVC.Config
  , selectProviderTimer :: String
  , selectedEstimatesObject :: CVC.Config
  , showAnim :: Boolean
  , animEndTime :: Int
  , isRentalSearch :: Boolean
}
