{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Beckn.Types.Storage.EntityTag where

import           Beckn.Types.App
import           Beckn.Types.Storage.RegistrationToken (RTEntityType (..))
import qualified Beckn.Utils.Defaults                  as Defaults
import           Data.Aeson
import           Data.Default
import           Data.Swagger
import           Data.Time
import           EulerHS.Prelude


import qualified Database.Beam                         as B

data EntityTagT f =
  EntityTag
    { _id               :: B.C f EntityTagId
    , _EntityId         :: B.C f Text
    , _entityType       :: B.C f Text
    , _TagId            :: B.C f Text
    , _TaggedBy         :: B.C f RTEntityType
    , _taggedByEntityId :: B.C f Text
    , _createdAt        :: B.C f LocalTime
    , _updatedAt        :: B.C f LocalTime
    , _info             :: B.C f (Maybe Text)
    }
  deriving (Generic, B.Beamable)

type EntityTag = EntityTagT Identity

type EntityTagPrimaryKey = B.PrimaryKey EntityTagT Identity

instance B.Table EntityTagT where
  data PrimaryKey EntityTagT f = EntityTagPrimaryKey (B.C f EntityTagId)
                               deriving (Generic, B.Beamable)
  primaryKey = EntityTagPrimaryKey . _id


instance Default EntityTag where
  def = EntityTag
    { _id         = EntityTagId Defaults.id
    , _EntityId = Defaults.id2
    , _entityType = "USER"
    , _TagId = Defaults.id3
    , _TaggedBy = USER
    , _taggedByEntityId = Defaults.id
    , _createdAt  = Defaults.localTime
    , _updatedAt  = Defaults.localTime
    , _info       = Nothing
    }

instance ToJSON EntityTag where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON EntityTag where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

deriving instance Show EntityTag

deriving instance Eq EntityTag

instance ToSchema EntityTag

insertExpression tag = insertExpressions [tag]

insertExpressions tags = B.insertValues tags

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity EntityTagT)
fieldEMod =
  B.setEntityName "entity_tag" <>
  B.modifyTableFields
    B.tableModification
      { _EntityId = "entity_id"
      , _entityType = "entity_type"
      , _TagId = "tag_id"
      , _TaggedBy = "tagged_by"
      , _taggedByEntityId = "tagged_by_entity_id"
      , _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }


