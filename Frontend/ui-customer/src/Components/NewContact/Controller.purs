module Components.NewContact.Controller where

import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader.Controller as GenericHeaderController
import Prelude
import Screens.Types (NewContacts)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import Data.String as DS
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Data.Array (filter, sortBy)
import Data.String (Pattern(..), contains)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import Debug

data Action
  = Click NewContacts
  | NoAction
  | ContactSelected Int
