module Domain.Action.Cac where

import Data.Aeson
import Environment (Flow)
import Kernel.Prelude

data CacTypeValidationReq = CacTypeValidationReq
  { key :: Text,
    value :: Value
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data CacTypeValidationResp = CacTypeValidationResp
  { result :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

typeCheckHandler :: CacTypeValidationReq -> Flow CacTypeValidationResp
typeCheckHandler _ = do
  result <- return False -- Here Goes Akhilesh's logic
  return $ CacTypeValidationResp result
