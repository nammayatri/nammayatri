module Domain.Types.FleetOperatorTripAction
  ( FleetOperatorTripAction (..),
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import EulerHS.Prelude

data FleetOperatorTripAction
  = TripStart
  | TripEnd
  | TripReset
  | TripRollback
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToSchema)

instance Aeson.ToJSON FleetOperatorTripAction where
  toJSON = \case
    TripStart -> Aeson.String "start"
    TripEnd -> Aeson.String "end"
    TripReset -> Aeson.String "reset"
    TripRollback -> Aeson.String "rollback"

instance Aeson.FromJSON FleetOperatorTripAction where
  parseJSON = AesonTypes.withText "FleetOperatorTripAction" $ \t ->
    case T.toLower t of
      "start" -> pure TripStart
      "end" -> pure TripEnd
      "reset" -> pure TripReset
      "rollback" -> pure TripRollback
      other -> fail $ "Unknown FleetOperatorTripAction: " <> T.unpack other
