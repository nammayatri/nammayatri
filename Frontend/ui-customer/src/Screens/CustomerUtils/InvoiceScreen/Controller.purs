module Screens.InvoiceScreen.Controller where

import Prelude (class Show, bind, pure, unit, ($), discard)
import Screens.Types (InvoiceScreenState)
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM (Eval, exit, continue, continueWithCmd)
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeaderController
import JBridge (generatePDF)
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

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
        _ <- pure $ generatePDF state "NEW"
        pure NoAction
    ]

eval _ state = continue state
