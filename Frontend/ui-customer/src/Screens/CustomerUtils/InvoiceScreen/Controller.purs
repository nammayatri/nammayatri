{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.InvoiceScreen.Controller where

import Prelude (class Show, bind, pure, unit, ($), discard, void)
import Screens.Types (InvoiceScreenState)
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM (Eval, exit, continue, continueWithCmd)
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeaderController
import Foreign (unsafeToForeign)
import JBridge (generatePDF)
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEventWithMultipleParams)
import Foreign (unsafeToForeign)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen INVOICE_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen INVOICE_SCREEN)
      trackAppEndScreen appId (getScreen INVOICE_SCREEN)
    GenericHeaderAC act -> case act of
      GenericHeaderController.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen INVOICE_SCREEN) "generic_header_action" "back_icon_onclick"
        trackAppEndScreen appId (getScreen INVOICE_SCREEN)
      _ -> pure unit
    PrimaryButtonAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen INVOICE_SCREEN) "primary_button" "download_pdf"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen INVOICE_SCREEN) "primary_button" "no_action"
    NoAction -> trackAppScreenEvent appId (getScreen INVOICE_SCREEN) "in_screen" "no_action"

data Action
  = PrimaryButtonAC PrimaryButton.Action
  | GenericHeaderAC GenericHeaderController.Action
  | BackPressed
  | NoAction
  | AfterRender

data ScreenOutput
  = GoBack
  | GoToHome

eval :: Action -> InvoiceScreenState -> Eval Action ScreenOutput InvoiceScreenState
eval BackPressed state = do
  if state.props.fromHomeScreen then
    exit GoToHome
  else
    exit GoBack

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ do pure BackPressed ]

eval (PrimaryButtonAC (PrimaryButton.OnClick)) state = do
  continueWithCmd state
    [ do
        void $ logEventWithMultipleParams state.data.logField "ny_user_invoice_download"
          $ [ { key: "Base fare", value: unsafeToForeign state.data.selectedItem.baseFare }
            , { key: "Distance", value: unsafeToForeign state.data.selectedItem.baseDistance }
            , { key: "Driver pickup charges", value: unsafeToForeign "₹ 10" }
            , { key: "Total fare", value: unsafeToForeign state.data.selectedItem.totalAmount }
            , { key: "Ride completion timestamp", value: unsafeToForeign state.data.selectedItem.rideEndTime }
            ]
        _ <- pure $ generatePDF state "NEW"
        pure NoAction
    ]

eval _ state = continue state
