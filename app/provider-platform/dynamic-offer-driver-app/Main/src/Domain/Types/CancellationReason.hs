{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.CancellationReason where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

data CancellationReason = CancellationReason
  { reasonCode :: CancellationReasonCode,
    description :: Text,
    enabled :: Bool,
    priority :: Int
  }
  deriving (Generic)

data CancellationReasonAPIEntity = CancellationReasonAPIEntity
  { reasonCode :: CancellationReasonCode,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeCancellationReasonAPIEntity :: CancellationReason -> CancellationReasonAPIEntity
makeCancellationReasonAPIEntity CancellationReason {..} =
  CancellationReasonAPIEntity {..}
