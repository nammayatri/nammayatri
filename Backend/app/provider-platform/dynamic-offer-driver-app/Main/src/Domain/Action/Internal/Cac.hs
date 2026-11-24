-- | Kept as a success stub after haskell-cac removal for backward compatibility:
-- older CAC tooling may still call /internal/typeCheck. Real schema checks are
-- gone — always return { result: true }.
module Domain.Action.Internal.Cac where

import Data.Aeson
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
typeCheckHandler _req = pure $ CacTypeValidationResp True
