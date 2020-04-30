{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Beckn.Types.Storage.Document where

import           Beckn.Types.App
import qualified Beckn.Utils.Defaults as Defaults
import           Data.Aeson
import           Data.Default
import           Data.Swagger
import           Data.Time
import           EulerHS.Prelude


import qualified Database.Beam        as B

data DocumentT f =
  Document
    { _id        :: B.C f DocumentId
    , _fileUrl   :: B.C f Text
    , _size      :: B.C f Int -- in bytes
    , _fileHash  :: B.C f Text -- md5 hash
    , _fileName  :: B.C f Text
    , _format    :: B.C f Text
    , _info      :: B.C f (Maybe Text)
    , _createdAt :: B.C f LocalTime
    , _updatedAt :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type Document = DocumentT Identity

type DocumentPrimaryKey = B.PrimaryKey DocumentT Identity

instance B.Table DocumentT where
  data PrimaryKey DocumentT f = DocumentPrimaryKey (B.C f DocumentId)
                               deriving (Generic, B.Beamable)
  primaryKey = DocumentPrimaryKey . _id


instance Default Document where
  def = Document
    { _id         = DocumentId Defaults.id
    , _fileUrl = "/home/beckn-epass/data/IMG_000.jpg"
    , _fileName = "IMG_000.png"
    , _fileHash = "7243e5959c33e55fa033868bb241ea89"
    , _format = "JPG"
    , _size = 1024000
    , _createdAt  = Defaults.localTime
    , _updatedAt  = Defaults.localTime
    , _info       = Nothing
    }

instance ToJSON Document where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Document where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

deriving instance Show Document

deriving instance Eq Document

instance ToSchema Document

insertExpression tag = insertExpressions [tag]

insertExpressions tags = B.insertValues tags

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DocumentT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _fileUrl = "file_url"
      , _fileHash = "file_hash"
      , _fileName = "file_name"
      , _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }
