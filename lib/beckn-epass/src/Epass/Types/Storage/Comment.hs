{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Epass.Types.Storage.Comment where

import Data.Aeson
import Data.Default
import Data.Swagger
import Data.Time
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Storage.RegistrationToken (RTEntityType (..))
import qualified Epass.Utils.Defaults as Defaults
import EulerHS.Prelude

data CommentT f = Comment
  { _id :: B.C f CommentId,
    _CommentedOnEntityId :: B.C f Text,
    _commentedOnEntityType :: B.C f Text,
    _CommentedBy :: B.C f Text,
    _commentedByEntityType :: B.C f RTEntityType,
    _value :: B.C f Text,
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime,
    _info :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type Comment = CommentT Identity

type CommentPrimaryKey = B.PrimaryKey CommentT Identity

instance B.Table CommentT where
  data PrimaryKey CommentT f = CommentPrimaryKey (B.C f CommentId)
    deriving (Generic, B.Beamable)
  primaryKey = CommentPrimaryKey . _id

instance Default Comment where
  def =
    Comment
      { _id = CommentId Defaults.id,
        _CommentedOnEntityId = Defaults.id2,
        _commentedOnEntityType = "DOCUMENT",
        _CommentedBy = Defaults.id,
        _commentedByEntityType = USER,
        _value = "comment",
        _createdAt = Defaults.localTime,
        _updatedAt = Defaults.localTime,
        _info = Nothing
      }

instance ToJSON Comment where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Comment where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

deriving instance Show Comment

deriving instance Eq Comment

instance ToSchema Comment

insertExpression tag = insertExpressions [tag]

insertExpressions tags = B.insertValues tags

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CommentT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _CommentedOnEntityId = "commented_on_entity_id",
        _commentedOnEntityType = "commented_on_entity_type",
        _CommentedBy = "commented_by",
        _commentedByEntityType = "commented_by_entity_type",
        _createdAt = "created_at",
        _updatedAt = "updated_at"
      }
