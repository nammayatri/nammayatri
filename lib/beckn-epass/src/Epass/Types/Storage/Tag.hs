{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Epass.Types.Storage.Tag where

import Data.Aeson
import Data.Default
import Data.Swagger hiding (Tag)
import Data.Time
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Storage.RegistrationToken (RTEntityType (..))
import qualified Epass.Utils.Defaults as Defaults
import EulerHS.Prelude

data TagT f = Tag
  { _id :: B.C f TagId,
    _tagType :: B.C f Text,
    _tag :: B.C f Text,
    _CreatedBy :: B.C f Text,
    _createdByEntityType :: B.C f RTEntityType,
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime,
    _info :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type Tag = TagT Identity

type TagPrimaryKey = B.PrimaryKey TagT Identity

instance B.Table TagT where
  data PrimaryKey TagT f = TagPrimaryKey (B.C f TagId)
    deriving (Generic, B.Beamable)
  primaryKey = TagPrimaryKey . _id

instance Default Tag where
  def =
    Tag
      { _id = TagId Defaults.id,
        _tagType = "tag_type",
        _tag = "",
        _createdByEntityType = USER,
        _CreatedBy = Defaults.id,
        _createdAt = Defaults.localTime,
        _updatedAt = Defaults.localTime,
        _info = Nothing
      }

instance ToJSON Tag where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Tag where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

deriving instance Show Tag

deriving instance Eq Tag

instance ToSchema Tag

insertExpression tag = insertExpressions [tag]

insertExpressions tags = B.insertValues tags

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TagT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _tagType = "tag_type",
        _CreatedBy = "created_by",
        _createdByEntityType = "created_by_entity_type",
        _createdAt = "created_at",
        _updatedAt = "updated_at"
      }
