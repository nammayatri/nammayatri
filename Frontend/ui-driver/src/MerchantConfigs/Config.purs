module MerchantConfig where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Foreign.Generic (decode, encode)
import JBridge (getConfig)
import MerchantConfig.Types (AppConfig)
import MerchantConfig.DefaultConfig (config) as DefaultConfig
import Types.App (FlowBT)

getAppConfig :: FlowBT String AppConfig
getAppConfig = liftFlowBT $ getAppConfigEff

getAppConfigEff :: Effect AppConfig
getAppConfigEff  = do
  config' <- getConfig
  pure $
    case config' of
      Just config -> do
        case runExcept (decode (encode config )) of
            Right (_ :: AppConfig) -> config
            Left _ -> DefaultConfig.config
      Nothing -> do
            DefaultConfig.config

