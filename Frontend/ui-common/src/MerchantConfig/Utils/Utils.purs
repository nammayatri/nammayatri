module MerchantConfig.Utils where

import Prelude
import Common.Types.App (LazyCheck(..))
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Foreign.Generic (class Decode, class Encode, decode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import Debug
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Foreign.Generic (decode, encode)
import JBridge (getConfig)
import MerchantConfig.Types (AppConfig)
import MerchantConfig.DefaultConfig as DefaultConfig
import Types.App (FlowBT)

foreign import getStringFromConfig :: String -> String

foreign import getValueFromConfig :: forall a. String -> a

foreign import getENStrings :: String -> String

foreign import getMerchantId :: String -> Foreign

data Merchant
  = NAMMAYATRI
  | JATRISAATHI
  | YATRI
  | PAYTM

derive instance genericMerchant :: Generic Merchant _

instance eqMerchant :: Eq Merchant where
  eq = genericEq

instance encodeMerchant :: Encode Merchant where
  encode = defaultEnumEncode

instance decodeMerchant :: Decode Merchant where
  decode = defaultEnumDecode

getMerchant :: LazyCheck -> Merchant
getMerchant lazy = case decodeMerchantId (getMerchantId "") of
  Just merchant -> merchant
  Nothing -> NAMMAYATRI

decodeMerchantId :: Foreign -> Maybe Merchant
decodeMerchantId = hush <<< runExcept <<< decode

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