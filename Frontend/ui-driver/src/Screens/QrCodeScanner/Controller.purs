module Screens.QrCodeScanner.Controller where

import Prelude
import Screens.Types as ST
import Debug (spy)
import Engineering.Helpers.Commons
import PrestoDOM (class Loggable, Eval, update, continue, exit, continueWithCmd, updateAndExit)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        _ -> pure unit

data Action = 
        NoAction
    |   GoBack
    |   StartQRScanner String String

data ScreenOutput 
  = GoToHomeScreen 
  | ExecuteCallback String

eval :: Action -> ST.QrCodeScannerState -> Eval Action ScreenOutput ST.QrCodeScannerState

eval GoBack state = exit $ GoToHomeScreen

eval (StartQRScanner isError qrData) state = do
    if isError == "False" then
        continue state
    else
        exit $ ExecuteCallback qrData

eval _ state = update state