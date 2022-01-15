module Core.Payment
  ( Payment (..),
    PaymentType (..),
    TLMethod (..),
    Params (..),
  )
where

import Beckn.Types.Core.Migration.DecimalValue (DecimalValue (..))
-- import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.Example
import Beckn.Utils.JSON

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
--          import Data.HashMap.Strict
import Data.Text
import GHC.Generics
--          import EulerHS.Prelude hiding (State, (.=))
import Servant.Client (BaseUrl, parseBaseUrl)

data Payment = Payment
  { uri :: Maybe BaseUrl,
    tl_method :: Maybe TLMethod,
    params :: Maybe Params,
    _type :: Maybe PaymentType,
    status :: Maybe Status
    -- time :: Maybe Time
  }
  deriving (Generic, Show)

data TLMethod = HttpGet | HttpPost
  deriving (Show)

instance FromJSON TLMethod where
  parseJSON (String "http/get") = pure HttpGet
  parseJSON (String "http/post") = pure HttpPost
  parseJSON e = typeMismatch "tl_method string" e

instance ToJSON TLMethod where
  toJSON HttpGet = String "http/get"
  toJSON HttpPost = String "http/post"

options :: Options
options = defaultOptions { omitNothingFields = True }

data Params = Params
  { 
    transaction_id :: Maybe Text,
    transaction_status :: Maybe TrStatus,
    amount :: Maybe DecimalValue,
    currency :: Text
    --            additional :: HashMap Text Text
  }
  deriving (Generic, Eq, Show, FromJSON)

instance ToJSON Params where
  toJSON = genericToJSON options


data TrStatus  = Captured
               | Failed
               | Payment_link_created
               | Payment_link_expired
               | Payment_link_issued
               | Pefunded
  deriving (Generic, Eq, Show, FromJSON)

instance ToJSON TrStatus where
  toJSON = genericToJSON constructorsToLowerOptions

-- instance FromJSON Params where
--   parseJSON = withObject "Params" $ \o ->
    -- Params
    --   <$> o .: "transaction_id"
    --   <*> o .: "transaction_status"
    --   <*> o .: "amount"
    --   <*> o .: "currency"
    --   <*> mapM f (additional o)
    -- where
    --   f (String val) = pure val
    --   f e = typeMismatch "additional property of Params" e
    --   additional =
        -- delete "transaction_id"
        --   . delete "transaction_status"
        --   . delete "amount"
        --   . delete "currency"
-- 
-- instance ToJSON Params where
--   toJSON Params {..} = uniteObjects [object knownParams, Object (String <$> additional)]
    -- where
    --   knownParams =
        -- [ "transaction_id" .= transaction_id,
        --   "transaction_status" .= transaction_status,
        --   "amount" .= amount,
        --   "currency" .= currency
        -- ]
-- 
--        data BankAccount = BankAccount
--          { ifsc_code :: Maybe Text,
           -- account_number :: Maybe Text,
           -- account_holder_name :: Maybe Text
--          }
--          deriving (Generic, FromJSON, ToJSON, Eq, Show)

data PaymentType
  = ON_ORDER
  | PRE_FULFILLMENT
  | ON_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Generic, Eq, Show)

data Status = PAID | NOT_PAID
  deriving (Generic, Eq, Show)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON  stripPrefixUnderscoreIfAny { omitNothingFields = True }

instance Example Payment where
  example =
    Payment
      { uri = parseBaseUrl "https://www.payment_url.com/",
        tl_method = Just HttpGet,
        params = Just (Params
                        -- Nothing
                        -- Nothing
                        (Just "payment_transaction_id")
                        (Just Payment_link_created)
                        (Just (DecimalValue "30"))
                        "INR"
                      ),
        _type = Just PRE_FULFILLMENT,
        status = Just PAID
        -- time = Nothing
      }

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Status where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON Status where
  toJSON = genericToJSON constructorsWithHyphens

