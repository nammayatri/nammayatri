{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Beckn.Types.Storage.Comment where

import           Beckn.Types.App
import qualified Beckn.Utils.Defaults as Defaults
import           Data.Aeson
import           Data.Default
import           Data.Swagger
import           Data.Time
import           EulerHS.Prelude


import qualified Database.Beam        as B

data CommentT f =
  Comment
    { _id                    :: B.C f CommentId
    , _PrimaryEntityId       :: B.C f Text
    , _primaryEntityType     :: B.C f Text
    , _CommentedEntityId     :: B.C f Text
    , _CommentedBy           :: B.C f Text
    , _commentedByEntityType :: B.C f Text
    , _value                 :: B.C f Text
    , _createdAt             :: B.C f LocalTime
    , _updatedAt             :: B.C f LocalTime
    , _info                  :: B.C f (Maybe Text)
    }
  deriving (Generic, B.Beamable)

type Comment = CommentT Identity

type CommentPrimaryKey = B.PrimaryKey CommentT Identity

instance B.Table CommentT where
  data PrimaryKey CommentT f = CommentPrimaryKey (B.C f CommentId)
                               deriving (Generic, B.Beamable)
  primaryKey = CommentPrimaryKey . _id


instance Default Comment where
  def = Comment
    { _id         = CommentId Defaults.id
    , _PrimaryEntityId = Defaults.id2
    , _primaryEntityType = "DOCUMENT"
    , _CommentedEntityId = Defaults.id3
    , _CommentedBy = Defaults.id
    , _commentedByEntityType = "USER"
    , _value = "comment"
    , _createdAt  = Defaults.localTime
    , _updatedAt  = Defaults.localTime
    , _info       = Nothing
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
      { _PrimaryEntityId = "primary_entity_id"
      , _primaryEntityType = "primary_entity_type"
      , _CommentedEntityId = "commented_by_entity_id"
      , _CommentedBy = "comment_by"
      , _commentedByEntityType = "commented_by_entity_type"
      , _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }


