{-# LANGUAGE StandaloneDeriving #-}

module Beckn.Types.Storage.Issue where

import Beckn.Types.ID
import Data.Aeson
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude

data IssueT f = Issue
  { _id :: B.C f (ID Issue),
    _customerId :: B.C f Text,
    _productInstanceId :: B.C f (Maybe Text),
    _contactEmail :: B.C f Text,
    _reason :: B.C f Text,
    _description :: B.C f (Maybe Text),
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Issue = IssueT Identity

type IssuePrimaryKey = B.PrimaryKey IssueT Identity

instance B.Table IssueT where
  data PrimaryKey IssueT f = IssuePrimaryKey (B.C f (ID Issue))
    deriving (Generic, B.Beamable)
  primaryKey = IssuePrimaryKey . _id

deriving instance Show Issue

deriving instance Eq Issue

deriving instance ToJSON Issue

deriving instance FromJSON Issue

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity IssueT)
fieldEMod =
  B.setEntityName "issues"
    <> B.modifyTableFields
      B.tableModification
        { _customerId = "customer_id",
          _productInstanceId = "product_instance_id",
          _contactEmail = "contact_email",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
