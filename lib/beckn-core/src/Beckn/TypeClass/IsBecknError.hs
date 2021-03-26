module Beckn.TypeClass.IsBecknError where

import qualified Beckn.Types.Core.Error as Error
import Data.Aeson.Types
import EulerHS.Prelude hiding ((.=))

class IsBecknError beckn_error where
  toBecknError :: beckn_error -> BecknAPIError

newtype BecknAPIError = BecknAPIError Error.Error
  deriving (Generic, Eq, Show)

instance FromJSON BecknAPIError where
  parseJSON (Object v) = BecknAPIError <$> v .: "error"
  parseJSON invalid =
    prependFailure
      "Parsing BecknAPIError failed, "
      (typeMismatch "Object" invalid)

instance ToJSON BecknAPIError where
  toJSON (BecknAPIError err) = object ["message" .= ack, "error" .= err]
    where
      ack = object ["ack" .= status]
      status = object ["status" .= ("NACK" :: Text)]
