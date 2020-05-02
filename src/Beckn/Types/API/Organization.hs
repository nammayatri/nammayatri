module Beckn.Types.API.Organization where

import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Comment      as SC
import qualified Beckn.Types.Storage.Document     as SD
import           Beckn.Types.Storage.Organization
import qualified Beckn.Types.Storage.Tag          as ST
import           Data.Default
import           Data.Swagger
import           Data.Time
import           EulerHS.Prelude


data CreateOrganizationReq =
  CreateOrganizationReq
    { _name    :: Text
    , _gstin   :: Maybe Text
    , _city    :: Text
    , _state   :: Text
    , _country :: Text
    , _pincode :: Int
    , _address :: Text
    }
  deriving (Generic, ToSchema)

instance FromJSON CreateOrganizationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data OrganizationRes =
  OrganizationRes
    { organization :: Organization
    }
  deriving (Generic, ToJSON, ToSchema)

data GetOrganizationRes =
  GetOrganizationRes
    { organization :: OrgInfo
    }
  deriving (Generic, ToJSON, ToSchema)


data ListOrganizationReq =
  ListOrganizationReq
   { _limit  :: Int
   , _offset :: Int
   , __type  :: Text
   } deriving (Generic, ToSchema)

instance FromJSON ListOrganizationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListOrganizationRes =
  ListOrganizationRes
   { _organizations :: [OrgInfo]
   } deriving (Generic, ToSchema)

data OrgInfo =
  OrgInfo
    { _id                        :: OrganizationId
    , _gstin                     :: (Maybe Text)
    , _status                    :: Status
    , _verified                  :: Bool
    , _Tags                      :: [ST.Tag]
    , _Documents                 :: [SD.Document]
    , _Comments                  :: [SC.Comment]
    , _isBlacklistedOrganization :: Bool
    , _isBlacklistedLocation     :: Bool
    , _status                    :: Status
    , _location                  :: Location
    , _info                      :: (Maybe Text)
    , _createdAt                 :: LocalTime
    , _updatedAt                 :: LocalTime
    }
      deriving (Generic, ToSchema)

instance ToJSON ListOrganizationRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance ToJSON OrgInfo where
  toJSON = genericToJSON stripLensPrefixOptions


data UpdateOrganizationReq =
  UpdateOrganizationReq
    { _status :: Status
    } deriving (Generic, ToSchema)

instance FromJSON UpdateOrganizationReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions
