module Domain.Types.CancellationReason where

import Beckn.Prelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Servant

data CancellationStage = OnSearch | OnConfirm | OnAssign
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData CancellationStage where
  parseUrlPiece = parseHeader . encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving (Generic, Show, Eq, Read, ToJSON, FromJSON, ToSchema)

data CancellationReason = CancellationReason
  { reasonCode :: CancellationReasonCode,
    description :: Text,
    enabled :: Bool,
    onSearch :: Bool,
    onConfirm :: Bool,
    onAssign :: Bool
  }
  deriving (Generic, Show)

data CancellationReasonAPIEntity = CancellationReasonAPIEntity
  { reasonCode :: CancellationReasonCode,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeCancellationReasonAPIEntity :: CancellationReason -> CancellationReasonAPIEntity
makeCancellationReasonAPIEntity CancellationReason {..} =
  CancellationReasonAPIEntity {..}
