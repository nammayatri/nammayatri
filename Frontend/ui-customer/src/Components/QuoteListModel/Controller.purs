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
import Screens.Types (TipViewProps, QuoteListItemState(..), FareProductType(..))
import MerchantConfig.Types (AppConfig)
import Components.TipsView as TipsView
import Components.ProviderModel as PM
import Components.ChooseVehicle as ChooseVehicle
import Components.ChooseYourRide (BookAnyProps(..))
import ConfigProvider
import Prelude
import Data.Maybe
import Data.Array (length)
import Common.Types.App as CTA

data Action = GoBack
            | NoAction TipViewProps
            | PrimaryButtonActionController PrimaryButtonController.Action
            | QuoteListItemActionController QuoteListItemController.Action
            | CancelAutoAssigning
            | TipViewPrimaryButtonClick PrimaryButtonController.Action
            | HidePopUp
            | ChangeTip
            | TipsViewActionController TipsView.Action
            | ProviderModelAC PM.Action
            | CancelTimer
            | BoostSearchButtonClick PrimaryButtonController.Action
            | ServicesOnClick ChooseVehicle.Config String
            | TipBtnClick Int Int TipViewProps
            | ChooseVehicleAC ChooseVehicle.Action
            | BoostSearchAction PrimaryButtonController.Action
            | GotItAction PrimaryButtonController.Action
            | ShowBookAnyInfo
            | CloseBoostSearch 

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
  , city :: CTA.City
  , customerTipArray :: Array String
  , customerTipArrayWithValues :: Array Int
  , providerSelectionStage :: Boolean
  , quoteList :: Array ChooseVehicle.Config
  , selectProviderTimer :: String
  , selectedEstimatesObject :: ChooseVehicle.Config
  , showAnim :: Boolean
  , animEndTime :: Int
  , isRentalSearch :: Boolean
  , hasToll :: Boolean
  , showBookAnyOptions :: Boolean
  , showBoostSearch :: Boolean
  , boostSearchEstimate :: ChooseVehicle.Config
  , fareProductType :: FareProductType
}

getPriceWithTip :: BookAnyProps -> ChooseVehicle.Config -> Array ChooseVehicle.Config -> Int -> String
getPriceWithTip bookAnyProps estimate estimates tip = 
  let currency = getCurrency appConfig
      minPrice = bookAnyProps.minPrice + tip
      maxPrice = bookAnyProps.maxPrice + tip
      estimateMinPrice = fromMaybe 0 estimate.minPrice
      estimateMaxPrice = fromMaybe 0 estimate.maxPrice
  in case (length estimates), estimate.vehicleVariant == "BOOK_ANY" of 
      0, true -> "-"
      _, true -> if minPrice == maxPrice then (currency <> (show minPrice))
                 else (currency <> (show minPrice) <> " - " <> currency <> (show maxPrice))
      _ , false -> if (isNothing estimate.minPrice) && (isNothing estimate.maxPrice) then (show $ (estimate.basePrice + tip)) 
                   else if estimateMinPrice == estimateMaxPrice then (currency <> (show $ estimateMinPrice + tip))
                   else (currency <> (show $ estimateMinPrice + tip) <> " - " <> currency <> (show $ estimateMaxPrice + tip))
      _,_ -> "-"
