module Beckn.Types.API.Organization where

import           Beckn.Types.Storage.Organization
import           Data.Swagger
import           EulerHS.Prelude

data CreateOrganizationReq =
  CreateOrganizationReq
    { _name    :: Text
    , _gstin   :: Maybe Text
    , _city    :: Text
    , _state   :: Text
    , _country :: Text
    , _pincode :: Text
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
   { organizations :: [Organization]
   } deriving (Generic, ToJSON, ToSchema)

data UpdateOrganizationReq =
  UpdateOrganizationReq
    { status :: Status
    } deriving (Generic, FromJSON, ToSchema)
