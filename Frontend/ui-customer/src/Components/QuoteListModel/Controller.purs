module Components.QuoteListModel.Controller where

import Components.PrimaryButton as PrimaryButtonController
import Components.QuoteListItem as QuoteListItemController
import Data.Maybe (Maybe)

data Action = GoBack
            | NoAction
            | PrimaryButtonActionController PrimaryButtonController.Action
            | QuoteListItemActionController QuoteListItemController.Action
            | HomeButtonActionController PrimaryButtonController.Action
            | TryAgainButtonActionController PrimaryButtonController.Action
            | CancelAutoAssigning
            | HidePopUp

type QuoteListModelState = {
     source :: String
  , destination :: String
  , quoteListModel :: Array QuoteListItemController.QuoteListItemState
  , selectedQuote :: Maybe String
  , autoSelecting :: Boolean
  , searchExpire :: Int
}
