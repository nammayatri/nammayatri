{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Epass.Types.Storage.EntityDocument where

import Data.Aeson
import Data.Default
import Data.Swagger
import Data.Time
import qualified Database.Beam as B
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Utils.Defaults as Defaults
import EulerHS.Prelude

data EntityDocumentT f = EntityDocument
  { _id :: B.C f EntityDocumentId,
    _EntityId :: B.C f Text,
    _entityType :: B.C f DocumentEntity,
    _DocumentId :: B.C f Text,
    _documentType :: B.C f Text,
    _CreatedBy :: B.C f Text,
    _createdByEntityType :: B.C f DocumentEntity,
    _verified :: B.C f Bool,
    _VerifiedBy :: B.C f (Maybe Text),
    _verifiedByEntityType :: B.C f (Maybe DocumentEntity),
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime,
    _info :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type EntityDocument = EntityDocumentT Identity

type EntityDocumentPrimaryKey = B.PrimaryKey EntityDocumentT Identity

instance B.Table EntityDocumentT where
  data PrimaryKey EntityDocumentT f = EntityDocumentPrimaryKey (B.C f EntityDocumentId)
    deriving (Generic, B.Beamable)
  primaryKey = EntityDocumentPrimaryKey . _id

instance Default EntityDocument where
  def =
    EntityDocument
      { _id = EntityDocumentId Defaults.id,
        _EntityId = Defaults.id2,
        _entityType = USER,
        _DocumentId = Defaults.id3,
        _documentType = "IMAGE",
        _CreatedBy = Defaults.id,
        _createdByEntityType = USER,
        _verified = False,
        _VerifiedBy = Just Defaults.id,
        _verifiedByEntityType = Just USER,
        _createdAt = Defaults.localTime,
        _updatedAt = Defaults.localTime,
        _info = Nothing
      }

instance ToJSON EntityDocument where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON EntityDocument where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

deriving instance Show EntityDocument

deriving instance Eq EntityDocument

instance ToSchema EntityDocument

insertExpression tag = insertExpressions [tag]

insertExpressions tags = B.insertValues tags

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity EntityDocumentT)
fieldEMod =
  B.setEntityName "entity_document"
    <> B.modifyTableFields
      B.tableModification
        { _EntityId = "entity_id",
          _entityType = "entity_type",
          _DocumentId = "document_id",
          _documentType = "document_type",
          _CreatedBy = "created_by",
          _createdByEntityType = "created_by_entity_type",
          _VerifiedBy = "verified_by",
          _verifiedByEntityType = "verified_by_entity_type",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
