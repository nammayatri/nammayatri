module Config.Config where

import Prelude
import Config.Types
import Prelude
import Data.Maybe
import Effect (Effect)
import MerchantConfigs.DefaultConfig (config) as DefaultConfig
import Presto.Core.Types.Language.Flow (Flow)
import Helpers.Utils (getMerchantConfig)
import Foreign.Generic (decodeJSON)
import Presto.Core.Utils.Encoding 
import Data.Either (Either(..))
import Control.Monad.Except (runExcept)
import Data.Either (either, hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Generic (class Decode, decode, encode)
import Foreign.Index (readProp)
import Prelude (Unit, unit, (<=<), (<<<))
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Types.App (FlowBT)
import Debug.Trace (spy)
import Control.Monad.Except.Trans (lift)

liftFlow :: forall val z. (Effect val)  -> Flow z val
liftFlow effVal = doAff do liftEffect (effVal)

getAppConfig :: FlowBT String AppConfig
getAppConfig = lift $ lift $ liftFlow $ getAppConfig_ 

getAppConfig_ :: Effect AppConfig
getAppConfig_  = do
  config' <- getMerchantConfig
  _ <- pure $ spy "config' ---->>> "  config'
  pure $
    case config' of
      Just config -> do
        case runExcept (decode (encode config )) of
            Right (obj :: AppConfig) -> do
                let _ =  spy "config ---->>> right "  ""
                config
            Left err -> do
                let _  =  spy "config ---->>> left "  ""
                DefaultConfig.config
      Nothing -> do
            DefaultConfig.config

