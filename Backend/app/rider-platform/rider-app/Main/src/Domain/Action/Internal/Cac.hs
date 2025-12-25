module Domain.Action.Internal.Cac where

import Data.Aeson
import Data.Text (unpack)
import Domain.Types.CacType
import Environment (Flow)
import Kernel.Prelude

data CacTypeValidationReq = CacTypeValidationReq
  { key :: Text,
    value :: Value
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype CacTypeValidationResp = CacTypeValidationResp {result :: Bool}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

typeCheckHandler :: CacTypeValidationReq -> Flow CacTypeValidationResp
typeCheckHandler (CacTypeValidationReq key value) = return (CacTypeValidationResp $ checkParseCommon (unpack key, value))
