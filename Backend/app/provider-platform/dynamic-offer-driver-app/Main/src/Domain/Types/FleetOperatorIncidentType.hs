module Domain.Types.FleetOperatorIncidentType
  ( FleetOperatorIncidentType (..),
    toBmsAlertType,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as T
import Kernel.Prelude

data FleetOperatorIncidentType
  = Accident
  | BreakDown
  | Others
  deriving stock (Eq, Show, Generic, Ord, Read)
  deriving anyclass (ToSchema)

instance Aeson.ToJSON FleetOperatorIncidentType where
  toJSON = Aeson.String . toBmsAlertType

instance Aeson.FromJSON FleetOperatorIncidentType where
  parseJSON = AesonTypes.withText "FleetOperatorIncidentType" $ \t ->
    case T.toLower t of
      "accident" -> pure Accident
      "breakdown" -> pure BreakDown
      "others" -> pure Others
      other -> fail $ "Unknown FleetOperatorIncidentType: " <> T.unpack other

-- | Also the `alert_type` bms keys its rule and routing rows on, so these are a data contract.
toBmsAlertType :: FleetOperatorIncidentType -> Text
toBmsAlertType = \case
  Accident -> "accident"
  BreakDown -> "breakdown"
  Others -> "others"
