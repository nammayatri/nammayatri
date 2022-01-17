module Core.OnInit.Payment where

import Beckn.Prelude
import Beckn.Types.Core.Migration.DecimalValue (DecimalValue (..))
import Beckn.Utils.JSON
import Core.Payment hiding (Params, Payment)

data Payment = Payment
  { uri :: BaseUrl,
    tl_method :: TLMethod,
    params :: Params,
    _type :: PaymentType,
    status :: Status
  }
  deriving (Generic, Show)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Params = Params
  { amount :: DecimalValue,
    currency :: Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)
