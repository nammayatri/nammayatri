{-# LANGUAGE DerivingVia #-}

module Domain.Types.Merchant.MerchantConfigNew where

import qualified Data.Aeson as A
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text.Encoding as TE
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import qualified Debug.Trace as T
import Domain.Types.Common
import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common ()
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Time (Seconds)

data MerchantConfigNewD (s :: UsageSafety) = MerchantConfigNew
  { merchantId :: Id Merchant,
    name :: Text,
    geofencingConfig :: GeofencingConfig,
    registryUrl :: BaseUrl,
    gatewayUrl :: BaseUrl,
    driverOfferBaseUrl :: BaseUrl,
    driverOfferApiKey :: Text,
    driverOfferMerchantId :: Text,
    city :: Context.City,
    geoHashPrecisionValue :: Int,
    country :: Context.Country,
    dirCacheSlot :: [Slot],
    timeDiffFromUtc :: Seconds,
    distanceWeightage :: Int,
    minimumDriverRatesCount :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

type MerchantConfigNew = MerchantConfigNewD 'Safe

instance FromJSON (MerchantConfigNewD 'Unsafe)

instance ToJSON (MerchantConfigNewD 'Unsafe)

data Slot = Slot
  { startTime :: TimeOfDay,
    endTime :: TimeOfDay,
    slot :: Int
  }
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be [Slot] where
  sqlValueSyntax = sqlValueSyntax . (A.String . TE.decodeUtf8 . toStrict . A.encode . A.toJSON)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Slot]

instance FromBackendRow Postgres [Slot] where
  fromBackendRow = do
    textVal <- fromBackendRow
    case T.trace (show textVal) $ A.fromJSON textVal of
      A.Success (jsonVal :: Text) -> case A.eitherDecode (fromStrict $ TE.encodeUtf8 jsonVal) of
        Right val -> pure val
        Left err -> fail ("Error Can't Decode Array of Domain slot :: Error :: " <> err)
      A.Error err -> fail ("Error Can't Decode Array of Domain slot :: Error :: " <> err)
