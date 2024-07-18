module Domain.Action.Internal.Cac where

import Data.Aeson
import Data.Text (unpack)
import Domain.Types.CacType
import Environment (Flow)
import Kernel.Prelude

data CacTypeValidationReq = CacTypeValidationReq
  { key :: Text,
    value :: Value,
    pushToDb :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype CacTypeValidationResp = CacTypeValidationResp {result :: Bool}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

typeCheckHandler :: CacTypeValidationReq -> Flow CacTypeValidationResp
typeCheckHandler req = do
  -- when (req.pushToDb == Just True) do

  return $ CacTypeValidationResp $ checkParseCommon (unpack (req.key), req.value)
