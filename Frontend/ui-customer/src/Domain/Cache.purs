module Domain.Cache where

import Prelude

import ConfigProvider (getAppConfigFlowBT)
import Constants as Constants
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import MerchantConfig.Types (AppConfig)
import ModifyScreenState (modifyScreenState)
import Types.App (FlowBT, GlobalState(..), ScreenType(..))

getAppConfigFromCache :: FlowBT String AppConfig
getAppConfigFromCache = do
  (GlobalState state) <- getState
  case state.appConfig of
    Just appConfig -> pure appConfig
    Nothing -> do
      config <- getAppConfigFlowBT Constants.appConfig
      modifyScreenState $ AppConfigType (\_ -> Just config)
      pure config