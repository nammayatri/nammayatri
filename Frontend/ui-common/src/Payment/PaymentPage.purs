module PaymentPage where

import Prelude
import Effect (Effect)
import Effect.Uncurried(EffectFn1)
import Effect.Aff(makeAff, nonCanceler)
import Common.Types.App (FlowBT) --Need to remove dependencies from app
import Control.Monad.Except.Trans (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Foreign.Generic (encodeJSON)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Presto.Core.Types.API (class StandardEncode, standardEncode)
import Foreign.Class (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Presto.Core.Utils.Encoding (defaultDecode,defaultEncode) 

type AffSuccess s = (s -> Effect Unit)
type MicroAPPInvokeSignature = String -> (AffSuccess String) ->  Effect Unit

foreign import checkPPInitiateStatus :: EffectFn1 (Unit -> Effect Unit) Unit
foreign import consumeBP :: EffectFn1 Unit Unit
foreign import startPP :: MicroAPPInvokeSignature
foreign import initiatePP :: EffectFn1 Unit Unit
foreign import getAvailableUpiApps :: EffectFn1 ((Array UpiApps) -> Effect Unit) Unit

type UpiApps
  = { supportsPay :: Boolean
    , supportsMandate :: Boolean
    , packageName :: String
    , appName :: String
    }

newtype PaymentPagePayload = PaymentPagePayload
  {
    requestId :: Maybe String,
    service :: Maybe String,
    payload :: PayPayload
  }

newtype PayPayload = PayPayload
  {
    action :: Maybe String,
    amount :: String,
    clientAuthToken :: String,
    clientAuthTokenExpiry :: String,
    clientId :: Maybe String,
    currency :: String,
    customerEmail :: Maybe String,
    customerId :: Maybe String,
    customerPhone :: Maybe String,
    description :: Maybe String,
    environment :: Maybe String,
    firstName :: Maybe String,
    lastName :: Maybe String,
    merchantId :: Maybe String,
    options_getUpiDeepLinks :: Maybe Boolean,
    orderId :: Maybe String,
    returnUrl :: Maybe String,
    "options.createMandate" :: Maybe String,
    "mandate.maxAmount" :: Maybe String,
    "mandate.endDate" :: Maybe String,
    "mandate.startDate" :: Maybe String,
    language :: Maybe String
  }

derive instance genericPayPayload :: Generic PayPayload _
derive instance newtypePayPayload :: Newtype PayPayload _
instance standardEncodePayPayload :: StandardEncode PayPayload where standardEncode (PayPayload id) = standardEncode id
instance showPayPayload :: Show PayPayload where show = genericShow
instance decodePayPayload :: Decode PayPayload where decode = defaultDecode
instance encodePayPayload :: Encode PayPayload where encode = defaultEncode

derive instance genericPaymentPagePayload :: Generic PaymentPagePayload _
derive instance newtypePaymentPagePayload :: Newtype PaymentPagePayload _
instance standardEncodePaymentPagePayload :: StandardEncode PaymentPagePayload where standardEncode (PaymentPagePayload id) = standardEncode id
instance showPaymentPagePayload :: Show PaymentPagePayload where show = genericShow
instance decodePaymentPagePayload :: Decode PaymentPagePayload where decode = defaultDecode
instance encodePaymentPagePayload :: Encode PaymentPagePayload where encode = defaultEncode

getPaymentPageLangKey :: String -> String 
getPaymentPageLangKey key = case key of 
  "EN_US" -> "english"
  "KN_IN" -> "kannada"
  "HI_IN" -> "hindi"
  "ML_IN" -> "malayalam"
  "BN_IN" -> "bengali"
  "TA_IN" -> "tamil"
  _       -> "english"

paymentPageUI :: forall st. PaymentPagePayload -> FlowBT String st String-- FlowBT String String
paymentPageUI payload = lift $ lift $ doAff $ makeAff (\cb -> (startPP (encodeJSON payload) (Right >>> cb) ) *> pure nonCanceler)