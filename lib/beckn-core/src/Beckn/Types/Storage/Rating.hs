{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.Rating where

import Beckn.Types.Id
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import Beckn.Utils.JSON
import Data.Swagger (ToSchema)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)

data RatingT f = Rating
  { id :: B.C f (Id Rating),
    productInstanceId :: B.C f (Id ProductInstance),
    ratingValue :: B.C f Int,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Rating = RatingT Identity

type RatingPrimaryId = B.PrimaryKey RatingT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table RatingT where
  data PrimaryKey RatingT f = RatingPrimaryKey (B.C f (Id Rating))
    deriving (Generic, B.Beamable)
  primaryKey = RatingPrimaryKey . id

deriving instance Show Rating

deriving instance Eq Rating

instance ToJSON Rating where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON Rating where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToSchema Rating

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RatingT)
fieldEMod =
  B.setEntityName "rating"
    <> B.modifyTableFields
      B.tableModification
        { productInstanceId = "product_instance_id",
          ratingValue = "rating_value",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
