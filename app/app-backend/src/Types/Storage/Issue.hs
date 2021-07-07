{-# LANGUAGE StandaloneDeriving #-}

module Types.Storage.Issue where

import Beckn.Types.Id
import Data.Aeson
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as ProdInst

data IssueT f = Issue
  { id :: B.C f (Id Issue),
    customerId :: B.C f (Id Person.Person),
    productInstanceId :: B.C f (Maybe (Id ProdInst.ProductInstance)),
    contactEmail :: B.C f Text,
    reason :: B.C f Text,
    description :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type Issue = IssueT Identity

type IssuePrimaryKey = B.PrimaryKey IssueT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table IssueT where
  data PrimaryKey IssueT f = IssuePrimaryKey (B.C f (Id Issue))
    deriving (Generic, B.Beamable)
  primaryKey = IssuePrimaryKey . id

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
        { customerId = "customer_id",
          productInstanceId = "product_instance_id",
          contactEmail = "contact_email",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
