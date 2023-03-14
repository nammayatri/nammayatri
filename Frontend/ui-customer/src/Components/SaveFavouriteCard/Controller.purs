module Components.SaveFavouriteCard.Controller where

import Components.PrimaryEditText.Controller as PrimaryEditTextController

data Action = SaveFavourite 
            | PrimayEditTA PrimaryEditTextController.Action 
            | OnClose
            | TagSelected String
            | NoAction