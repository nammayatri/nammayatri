module Epass.Types.API.Tag where

import Data.Default
import Data.Swagger hiding (Tag)
import Epass.Types.Storage.EntityTag
import Epass.Types.Storage.Tag
import EulerHS.Prelude

data CreateReq = CreateReq
  { _tagType :: Text,
    _tag :: Text,
    _info :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CreateReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

newtype CreateRes = CreateRes
  { _tag :: Tag
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON CreateRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON CreateRes where
  parseJSON = genericParseJSON stripLensPrefixOptions

newtype ListRes = ListRes
  { _tags :: [Tag]
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ListRes where
  parseJSON = genericParseJSON stripLensPrefixOptions

newtype ListVal = ListVal
  { _val :: [Text]
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON ListVal where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ListVal where
  parseJSON = genericParseJSON stripLensPrefixOptions

data TagEntityReq = TagEntityReq
  { _EntityId :: Text,
    _entityType :: Text,
    _TagId :: Text
  }
  deriving (Show, Generic, ToSchema)

instance ToJSON TagEntityReq where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON TagEntityReq where
  parseJSON = genericParseJSON stripLensPrefixOptions

newtype TagEntityRes = TagEntityRes
  { _entityTag :: EntityTag
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON TagEntityRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON TagEntityRes where
  parseJSON = genericParseJSON stripLensPrefixOptions
