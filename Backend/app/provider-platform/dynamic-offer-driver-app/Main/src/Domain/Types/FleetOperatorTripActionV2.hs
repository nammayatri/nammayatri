-- | transitV2 trip actions (GIMS /internal/fleet-operator/{gtfs_id}/v2/tripAction). No reset:
-- every run has its own trip rows, so there is nothing to reset. `cancel` / `uncancel` are
-- dashboard-only.
module Domain.Types.FleetOperatorTripActionV2
  ( FleetOperatorTripActionV2 (..),
    toGimsV2TripAction,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as T
import Kernel.Prelude
import Lib.GtfsDataServer.Types (GimsV2TripAction (..))

data FleetOperatorTripActionV2
  = TripV2Start
  | TripV2End
  | TripV2Rollback
  | TripV2Skip
  | TripV2Cancel
  | TripV2Uncancel
  deriving stock (Eq, Show, Generic, Ord, Read)
  deriving anyclass (ToSchema)

toGimsV2TripAction :: FleetOperatorTripActionV2 -> GimsV2TripAction
toGimsV2TripAction = \case
  TripV2Start -> GimsV2Start
  TripV2End -> GimsV2End
  TripV2Rollback -> GimsV2Rollback
  TripV2Skip -> GimsV2Skip
  TripV2Cancel -> GimsV2Cancel
  TripV2Uncancel -> GimsV2Uncancel

instance Aeson.ToJSON FleetOperatorTripActionV2 where
  toJSON = \case
    TripV2Start -> Aeson.String "start"
    TripV2End -> Aeson.String "end"
    TripV2Rollback -> Aeson.String "rollback"
    TripV2Skip -> Aeson.String "skip"
    TripV2Cancel -> Aeson.String "cancel"
    TripV2Uncancel -> Aeson.String "uncancel"

instance Aeson.FromJSON FleetOperatorTripActionV2 where
  parseJSON = AesonTypes.withText "FleetOperatorTripActionV2" $ \t ->
    case T.toLower t of
      "start" -> pure TripV2Start
      "end" -> pure TripV2End
      "rollback" -> pure TripV2Rollback
      "skip" -> pure TripV2Skip
      "cancel" -> pure TripV2Cancel
      "uncancel" -> pure TripV2Uncancel
      other -> fail $ "Unknown FleetOperatorTripActionV2: " <> T.unpack other
