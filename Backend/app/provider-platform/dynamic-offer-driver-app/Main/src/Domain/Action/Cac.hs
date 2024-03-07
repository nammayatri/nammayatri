module Domain.Action.Cac where

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

data CacTypeValidationResp = CacTypeValidationResp
  { result :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

typeCheckHandler :: CacTypeValidationReq -> Flow CacTypeValidationResp
typeCheckHandler req = return $ CacTypeValidationResp $ checkParseCommon (unpack (req.key), req.value) -- Here Goes Akhilesh's logic
