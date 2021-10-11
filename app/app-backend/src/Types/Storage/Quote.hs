{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Quote where

import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.SearchRequest as SearchRequest

data QuoteT f = Quote
  { id :: B.C f (Id Quote),
    requestId :: B.C f (Id SearchRequest.SearchRequest),
    price :: B.C f Amount,
    discount :: B.C f (Maybe Amount),
    providerId :: B.C f (Id Org.Organization),
    providerName :: B.C f Text,
    providerMobileNumber :: B.C f Text,
    providerCompletedRidesCount :: B.C f Int,
    distanceToNearestDriver :: B.C f Double,
    vehicleVariant :: B.C f Text,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Quote = QuoteT Identity

type QuotePrimaryKey = B.PrimaryKey QuoteT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table QuoteT where
  data PrimaryKey QuoteT f = QuotePrimaryKey (B.C f (Id Quote))
    deriving (Generic, B.Beamable)
  primaryKey a = QuotePrimaryKey a.id

deriving instance Show Quote

deriving instance Eq Quote

instance ToJSON Quote where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Quote where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Quote

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity QuoteT)
fieldEMod =
  B.setEntityName "quote"
    <> B.modifyTableFields
      B.tableModification
        { requestId = "request_id",
          providerName = "provider_name",
          providerCompletedRidesCount = "provider_completed_rides_count",
          providerMobileNumber = "provider_mobile_number",
          distanceToNearestDriver = "distance_to_nearest_driver",
          vehicleVariant = "vehicle_variant",
          providerId = "provider_id",
          createdAt = "created_at"
        }

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    estimatedPrice :: Amount,
    discount :: Maybe Amount,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    nearestDriverDistance :: Double,
    createdAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeQuoteAPIEntity :: Quote -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} =
  QuoteAPIEntity
    { estimatedPrice = price,
      agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      nearestDriverDistance = distanceToNearestDriver,
      ..
    }
