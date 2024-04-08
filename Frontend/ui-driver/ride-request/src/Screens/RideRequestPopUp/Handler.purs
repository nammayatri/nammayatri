module Screens.RideRequestPopUp.Handler where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Helpers.Accessor (_rideRequestPopUpScreen)
import Helpers.Commons (liftFlow)
import Presto.Core.Types.Language.Flow (Flow, getState, modifyState)
import PrestoDOM (getPushFn, showScreenWithNameSpace)
import PrestoDOM.Core (initUIWithNameSpace)
import PrestoDOM.List (preComputeListItem)
import Screens.RideRequestPopUp.Controller (ScreenOutput)
import Screens.RideRequestPopUp.View as View
import Types (OverlayData(..))

rideRequestPopUp :: Flow OverlayData ScreenOutput
rideRequestPopUp = do
  (OverlayData oState) <- validateHolder
  showScreenWithNameSpace $ View.screen oState.rideRequestPopUpScreen
  where
  validateHolder = do
    (OverlayData oState) <- getState
    if (not oState.rideRequestPopUpScreen.wasHolderCreated) then do
      push <- liftFlow $ getPushFn (Just "RideRequestPopUp") "RideRequestPopUp"
      item <- preComputeListItem $ View.sheetViewHolder push oState.rideRequestPopUpScreen
      modifyState \(OverlayData oState) -> OverlayData oState { rideRequestPopUpScreen { wasHolderCreated = true, holderView = item } }
    else
      pure (OverlayData oState)
