module Types.Cancel where

import EulerHS.Prelude


data CancellationReason
  = ByUser
  | ByDriver
  | ByOrganization
  | AllocationTimeExpired
  | NoDriversInRange
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON)