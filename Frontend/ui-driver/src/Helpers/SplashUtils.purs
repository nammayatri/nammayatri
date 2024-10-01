module Helpers.SplashUtils where

import Prelude
import Common.Types.App as CTA
import Screens.SplashScreen.Handler as SplashHand
import Engineering.Helpers.BackTrack as EHB
import Effect.Aff as EA
import Engineering.Helpers.Commons as EHC
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (delay)
import Helpers.Utils as HU
import Engineering.Helpers.Utils as EHU
import PrestoDOM.Core (terminateUI)
import Data.Maybe (Maybe(..))
import Types.App (FlowBT)
import DecodeUtil as DU

hideSplashAndCallFlow :: FlowBT String Unit -> FlowBT String Unit
hideSplashAndCallFlow flow = do
  hideLoaderFlow
  flow

hideLoaderFlow :: FlowBT String Unit
hideLoaderFlow = do
  EHB.liftFlowBT HU.hideSplash
  toggleSetupSplash false
  void $ lift $ lift $ EHU.toggleLoader false


toggleSetupSplash :: Boolean -> FlowBT String Unit
toggleSetupSplash = 
  if _ then do
    let _ = DU.setKeyInWindow "forceAppToNoInternetScreen" true
    SplashHand.splashScreen
  else do
    let _ = DU.setKeyInWindow "forceAppToNoInternetScreen" false
    pure unit