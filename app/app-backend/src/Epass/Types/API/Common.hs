{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Epass.Types.API.Common where

import Beckn.Types.Storage.Person
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import qualified Data.Swagger as SW
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Query (HasSqlEqualityCheck)
import Epass.Types.Storage.Blacklist
import Epass.Types.Storage.EntityTag
import Epass.Types.Storage.Location
import Epass.Types.Storage.Tag
import EulerHS.Prelude
import Servant
import Servant.Swagger

data UserInfo
  = UserInfo
      { _user :: Person,
        _locationInfo :: Maybe LocationInfo
      }
  deriving (Generic, Show)

instance ToJSON UserInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON UserInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

data LocationInfo
  = LocationInfo
      { _location :: Location,
        _blacklistInfo :: Maybe Blacklist,
        _tagInfo :: [TagInfo]
      }
  deriving (Show, Generic)

instance ToJSON LocationInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON LocationInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

data TagInfo
  = TagInfo
      { tag :: Tag,
        entityTag :: EntityTag
      }
  deriving (Show, Generic)

instance ToJSON TagInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON TagInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions
