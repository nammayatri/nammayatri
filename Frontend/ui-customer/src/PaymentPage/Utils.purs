module PaymentPage.Utils where

import Prelude
import Effect(Effect)
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Effect.Aff (makeAff, nonCanceler)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe)
import Control.Monad.Except (runExcept)
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign(Foreign, unsafeToForeign)
import Data.Generic.Rep (class Generic)
import Data.Newtype(class Newtype)
import Foreign.Class (class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultEncode,defaultDecode)
import Types.App (FlowBT)
import Control.Monad.Except.Trans (lift)
import Debug (spy)



-- foreign import openpaymentpage :: forall action. (action -> Effect Unit) -> String -> (String -> action) -> Effect Unit

-- foreign import startPP' = String -> (String -> Effect unit) -> Effect Unit

type AffSuccess s = (s -> Effect Unit)
type MicroAPPInvokeSignature = String -> (AffSuccess String) ->  Effect Unit


foreign import startPP1 :: MicroAPPInvokeSignature

startPP'' :: forall a. PaymentPagePayload -> Flow a String
startPP'' payload = do
  response <- doAff $ makeAff (\cb -> (startPP1 (encodeJSON payload) (Right >>> cb) ) *> pure nonCanceler)
  _ <- pure $ spy "inside triggerOTPBT 2" response
  let a = PaymentPageResponse { error : false 
  , errorCode : ""
  , errorMessage : ""
  , payload: unsafeToForeign{"status":{"value0":"charged"},
                                         "action":"paymentPage",
                                         "paymentInstrument":{"value0":"DUMMY"},
                                         "paymentInstrumentGroup":{"value0":"WALLET"},
                                         "otherInfo":{}}
  , requestId : "06270f2dd1ec42b3b519e74c897b9dd6"
  , service : "in.juspay.hyperpay"
  }
  pure $ response


startPP :: PaymentPagePayload -> FlowBT String String
startPP payload = do
  action <- lift $ lift $ startPP'' payload
  _ <- pure $ spy "inside triggerOTPBT 1" action
  pure action

newtype PaymentPagePayload =
    PaymentPagePayload
    { action :: String
    , merchantId :: String
    , signaturePayload :: String
    , orderDetails :: String
    , signature:: String
    , clientId:: String
    , merchantKeyId :: String
    , environment :: String
    }
derive instance genericPaymentPagePayload :: Generic PaymentPagePayload _
derive instance newtypePaymentPagePayload :: Newtype PaymentPagePayload _
instance encodePaymentPagePayload :: Encode PaymentPagePayload where encode = defaultEncode

newtype PaymentPageResponse =
  PaymentPageResponse
    { requestId :: String
    , service :: String
    , payload :: Foreign
    , errorMessage :: String
    , errorCode :: String
    , error :: Boolean
    }

derive instance genericPaymentPageResponse :: Generic PaymentPageResponse _
derive instance newtypePaymentPageResponse :: Newtype PaymentPageResponse _
instance decodePaymentPageResponse :: Decode PaymentPageResponse where decode = defaultDecode

dummyPayload :: PaymentPagePayload
dummyPayload =
    PaymentPagePayload
    { action : "paymentPage"
    , merchantId :"picasso"
    , signaturePayload :"{\"orderId\":\"1234567890\",\"amount\":\"1.00\", \"merchant_id\":\"idea_preprod\", \"customer_id\": \"1234567890\",\"mobile_number\": \"9234567890\",\"email_address\": \"test@gmail.com\",\"timestamp\":\"1573724015209\"}"
    , orderDetails :"{\"orderId\":\"1234567890\",\"amount\":\"1.00\", \"merchant_id\":\"idea_preprod\", \"customer_id\": \"1234567890\",\"mobile_number\": \"9234567890\",\"email_address\": \"test@gmail.com\",\"timestamp\":\"1573724015209\"}"
    , signature:""
    , clientId:"picasso"
    , merchantKeyId:"4794"
    , environment : "sandbox" -- sandbox | production
    }


data PaymentStatus = SUCCESS | BACKPRESSED | USERABORTED | PENDING | AUTHORIZING | AUTHENTICATION_FAILED
              | AUTHORIZATION_FAILED | API_FAILURE | NEW | UNKNOWN_

getStatus (PaymentPageResponse resp) =
  if not resp.error then SUCCESS
  else UNKNOWN_
    -- let
    --   getError
    --     | resp.status == "backpressed" = BACKPRESSED
    --     | resp.status == "user_aborted" = USERABORTED
    --     | resp.status == "pending_vbv" = PENDING
    --     | resp.status == "authorizing" = AUTHORIZING
    --     | resp.status == "authentication_failed" = AUTHENTICATION_FAILED
    --     | resp.status == "authorization_failed" = AUTHORIZATION_FAILED
    --     | resp.status == "api_failure" = API_FAILURE
    --     | resp.status == "new" = NEW
    --     | otherwise = UNKNOWN
    --   in getError
