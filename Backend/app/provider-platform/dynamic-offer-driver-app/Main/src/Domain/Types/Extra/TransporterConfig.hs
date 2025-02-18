{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.TransporterConfig where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Text (Text)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import GHC.Generics (Generic)
import Prelude

data AppletKey = SosAppletID | RentalAppletID | FleetAppletID deriving (Show, Read, Eq, Ord, Generic)

instance Hashable AppletKey

-- Central conversion functions from AppletKey to Text and vice versa
appletKeyToString :: AppletKey -> Text
appletKeyToString = \case
  SosAppletID -> "SosAppletID"
  RentalAppletID -> "RentalAppletID"
  FleetAppletID -> "FleetAppletID"

stringToAppletKey :: Text -> Maybe AppletKey
stringToAppletKey = \case
  "SosAppletID" -> Just SosAppletID
  "RentalAppletID" -> Just RentalAppletID
  "FleetAppletID" -> Just FleetAppletID
  _ -> Nothing

instance ToJSON AppletKey where
  toJSON = String . appletKeyToString

instance FromJSON AppletKey where
  parseJSON = withText "AppletKey" $ maybe (fail "Invalid AppletKey") pure . stringToAppletKey

instance ToJSONKey AppletKey where
  toJSONKey = toJSONKeyText appletKeyToString

instance FromJSONKey AppletKey where
  fromJSONKey = FromJSONKeyText $ \t -> maybe (error "Unknown AppletKey") id (stringToAppletKey t)

data ExotelMapping = ExotelMapping
  { exotelMap :: HM.HashMap AppletKey Text
  }
  deriving (Show, Read, Eq, Ord, Generic)

fromFieldExotel ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion ExotelMapping
fromFieldExotel f mbValue = do
  value <- fromField f mbValue
  case fromJSON value of
    Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance HasSqlValueSyntax be Value => HasSqlValueSyntax be ExotelMapping where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance FromField ExotelMapping where
  fromField = fromFieldExotel

instance BeamSqlBackend be => B.HasSqlEqualityCheck be ExotelMapping

instance FromBackendRow Postgres ExotelMapping

instance ToJSON ExotelMapping where
  toJSON = \case ExotelMapping m -> object ["exotelMap" .= m]

instance FromJSON ExotelMapping where
  parseJSON = withObject "ExotelMapping" $ \v -> ExotelMapping <$> v .: "exotelMap"
