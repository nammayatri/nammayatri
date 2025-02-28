module Screens.QrCodeScanner.Controller where

import Prelude
import Screens.Types as ST
import Debug (spy)
import Engineering.Helpers.Commons
import Helpers.Utils as HU
import Control.Monad.Trans.Class (lift)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Utils as EHU
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

eval GoBack state = do
    void $ pure $ HU.stopScanning unit
    exit $ GoToHomeScreen

eval (StartQRScanner isError qrData) state = do
    if isError == "true" then
        continue state
    else
        exit $ ExecuteCallback qrData

eval _ state = update state