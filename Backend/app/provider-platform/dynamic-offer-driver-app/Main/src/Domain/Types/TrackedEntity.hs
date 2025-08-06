{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Types.TrackedEntity where

import qualified Data.Text as T
import Kernel.Prelude
import qualified Text.Show (show)

data TrackedEntity = TrackedEntity
  { entityId :: Text,
    isActive :: Bool
  }
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

-- Custom Show instance for database serialization: "id:ACTIVE" or "id:CANCELLED"
instance Show TrackedEntity where
  show (TrackedEntity entId True) = T.unpack $ entId <> ":ACTIVE"
  show (TrackedEntity entId False) = T.unpack $ entId <> ":CANCELLED"

-- Custom Read instance for database deserialization
instance Read TrackedEntity where
  readsPrec _ input =
    case T.splitOn ":" (T.pack input) of
      [entId, "ACTIVE"] -> [(TrackedEntity entId True, "")]
      [entId, "CANCELLED"] -> [(TrackedEntity entId False, "")]
      _ -> []

-- Helper functions for working with TrackedEntity
mkActiveEntity :: Text -> TrackedEntity
mkActiveEntity entId = TrackedEntity entId True

mkCancelledEntity :: Text -> TrackedEntity
mkCancelledEntity entId = TrackedEntity entId False

isEntityActive :: TrackedEntity -> Bool
isEntityActive = isActive

isEntityCancelled :: TrackedEntity -> Bool
isEntityCancelled = not . isActive

-- Update status of a specific entity in a list
updateTrackedEntityStatus :: Text -> Bool -> [TrackedEntity] -> [TrackedEntity]
updateTrackedEntityStatus targetId newStatus =
  map (\te -> if entityId te == targetId then te {isActive = newStatus} else te)

-- Query helpers for extracting active/cancelled entities
getActiveEntities :: [TrackedEntity] -> [Text]
getActiveEntities = map entityId . filter isActive

getCancelledEntities :: [TrackedEntity] -> [Text]
getCancelledEntities = map entityId . filter (not . isActive)
