{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Quote where

import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import Data.Time
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Org
import Types.Storage.Person (Person)
import Types.Storage.Products (Products)
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Vehicle as Vehicle

data QuoteT f = Quote
  { id :: B.C f (Id Quote),
    requestId :: B.C f (Id SearchRequest.SearchRequest),
    productId :: B.C f (Id Products),
    estimatedFare :: B.C f Amount,
    discount :: B.C f (Maybe Amount),
    estimatedTotalFare :: B.C f Amount,
    providerId :: B.C f (Id Org.Organization),
    distance :: B.C f Double,
    distanceToNearestDriver :: B.C f Double,
    vehicleVariant :: B.C f Vehicle.Variant,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

--TODO: organizationId - -- need to point to primarykey

type Quote = QuoteT Identity

type QuotePrimaryKey = B.PrimaryKey QuoteT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table QuoteT where
  data PrimaryKey QuoteT f = QuotePrimaryKey (B.C f (Id Quote))
    deriving (Generic, B.Beamable)
  primaryKey = QuotePrimaryKey . id

deriving instance Show Quote

deriving instance Eq Quote

instance ToJSON Quote where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Quote where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity QuoteT)
fieldEMod =
  B.setEntityName "quote"
    <> B.modifyTableFields
      B.tableModification
        { requestId = "request_id",
          productId = "product_id",
          distanceToNearestDriver = "distance_to_nearest_driver",
          estimatedFare = "estimated_fare",
          estimatedTotalFare = "estimated_total_fare",
          providerId = "provider_id",
          vehicleVariant = "vehicle_variant",
          createdAt = "created_at"
        }

-- | ByOrganizationId OrganizationId
data ListById
  = ByApplicationId (Id SearchRequest.SearchRequest)
  | ById (Id Products)
  | ByCustomerId (Id Person)
